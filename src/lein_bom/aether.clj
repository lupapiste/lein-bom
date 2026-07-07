(ns lein-bom.aether
  (:require [cemerick.pomegranate.aether :as aether :refer [maven-central]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml])
  (:import (org.eclipse.aether.artifact ArtifactProperties)
           (org.eclipse.aether.resolution ArtifactDescriptorRequest ArtifactDescriptorResult ArtifactRequest)
           (org.eclipse.aether RepositorySystem RepositorySystemSession)
           (org.eclipse.aether.graph Dependency Exclusion)
           (org.eclipse.aether DefaultRepositorySystemSession)
           (org.eclipse.aether.artifact Artifact DefaultArtifact)
           (org.eclipse.aether.transfer AbstractTransferListener TransferEvent)
           (org.eclipse.aether.util.repository SimpleArtifactDescriptorPolicy)
           (java.util.concurrent Executors Callable Future)))


(def ^Artifact artifact #'aether/artifact)

(def mirror-selector-fn #'aether/mirror-selector-fn)

(def mirror-selector #'aether/mirror-selector)

(def make-repository #'aether/make-repository)

(def repository-system #'aether/repository-system)

(declare dep-spec*)

(defn exclusion-spec
  "Given an Aether Exclusion, returns a lein-style exclusion vector with the
   :exclusion in its metadata."
  [^Exclusion ex]
  (with-meta (-> ex bean dep-spec*) {:exclusion ex}))

(defn dep-spec*
  "Base function for producing lein-style dependency spec vectors for dependencies
   and exclusions."
  [{:keys [groupId artifactId version classifier extension scope optional exclusions]
    :or   {version    nil
           scope      "compile"
           optional   false
           exclusions nil}}]
  (let [group-artifact (apply symbol (if (= groupId artifactId)
                                       [artifactId]
                                       [groupId artifactId]))]
    (vec (concat [group-artifact]
                 (when version [version])
                 (when (and (seq classifier)
                            (not= "*" classifier))
                   [:classifier classifier])
                 (when (and (seq extension)
                            (not (#{"*" "jar"} extension)))
                   [:extension extension])
                 (when optional [:optional true])
                 (when (and scope
                            (not (str/blank? scope))
                            (not= scope "compile"))
                   [:scope scope])
                 (when (seq exclusions)
                   [:exclusions (vec (map exclusion-spec exclusions))])))))


(defn dep-spec
  "Given an Aether Dependency, returns a lein-style dependency vector with the
   :dependency and its corresponding artifact's :file in its metadata."
  [^Dependency dep]
  (let [artifact (.getArtifact dep)]
    (-> (merge (bean dep) (bean artifact))
        dep-spec*
        (with-meta {:dependency dep :file (.getFile artifact)}))))

(defn artifact-descriptors-repository-session [{:keys [repository-system local-repo offline? transfer-listener mirror-selector] :as args}]
  (let [session (aether/repository-session args)]
    (-> ^DefaultRepositorySystemSession session
        (.setArtifactDescriptorPolicy (new SimpleArtifactDescriptorPolicy false false)))))

;; ---------------------------------------------------------------------------
;; Parallel pre-warm of the BOM import graph (see prewarm-import-graph! below).
;; A minimal POM reader: just enough to find a POM's parent and its
;; <scope>import</scope> dependencyManagement entries so we can walk the graph.

(defn- elem-children [node tag]
  (filter #(and (map? %) (= tag (:tag %))) (:content node)))

(defn- elem-text [node]
  (some->> (:content node) (filter string?) (apply str) str/trim not-empty))

(defn- child-text [node tag]
  (some-> (first (elem-children node tag)) elem-text))

(defn- interpolate
  "Best-effort resolution of a version string that may be a ${property}."
  [v props pom-version]
  (cond
    (nil? v)                    nil
    (not (str/includes? v "${")) v
    :else (let [k (second (re-find #"\$\{(.+?)\}" v))]
            (cond
              (contains? #{"project.version" "pom.version"} k) pom-version
              (contains? props k)                              (get props k)
              :else                                            nil))))

(defn- pom-next-coords
  "Parse a POM file and return the [group artifact version] triples of its
   parent and any <scope>import</scope> dependencyManagement entries."
  [^java.io.File f]
  (try
    (let [proj        (xml/parse f)
          pom-version (or (child-text proj :version)
                          (some-> (first (elem-children proj :parent)) (child-text :version)))
          props       (into {} (for [pnode (elem-children proj :properties)
                                     kv    (:content pnode)
                                     :when (map? kv)]
                                 [(name (:tag kv)) (elem-text kv)]))
          parent      (when-let [p (first (elem-children proj :parent))]
                        (let [g (child-text p :groupId)
                              a (child-text p :artifactId)
                              v (child-text p :version)]
                          (when (and g a v) [[g a v]])))
          imports     (for [dm   (elem-children proj :dependencyManagement)
                            deps (elem-children dm :dependencies)
                            d    (elem-children deps :dependency)
                            :when (= "import" (child-text d :scope))
                            :let [g (child-text d :groupId)
                                  a (child-text d :artifactId)
                                  v (interpolate (child-text d :version) props pom-version)]
                            :when (and g a v)]
                        [g a v])]
      (concat parent imports))
    (catch Exception _ nil)))

;; println writes the message and the trailing newline as two separate ops, so
;; concurrent prewarm threads interleave them (merged lines, stray blanks). Hold
;; this monitor while printing so each line is emitted atomically.
(defonce ^:private print-lock (Object.))

(defn- prewarm-log [s]
  (locking print-lock
    (println s)))

(defn- resource->bom-coords
  "Turn an Aether resource path (…/com/google/cloud/libraries-bom/26.84.0/libraries-bom-26.84.0.pom)
   into a \"group/artifact:version\" label. Returns nil for non-POM resources
   (checksums, metadata) so only real BOM POMs get logged."
  [^String name]
  (when (.endsWith name ".pom")
    (let [parts (str/split name #"/")
          n     (count parts)]
      (when (>= n 4)
        (format "%s/%s:%s"
                (str/join "." (subvec parts 0 (- n 3)))   ; groupId
                (nth parts (- n 3))                        ; artifactId
                (nth parts (- n 2)))))))                   ; version

(defn- download-logging-transfer-listener
  "A TransferListener that logs each BOM POM once, only when it is actually
   fetched over the network. Cached POMs fire no transfer event, so they stay
   silent -- mirroring how Leiningen reports a dependency download. Fires on the
   connector's download threads, so it prints through the shared lock."
  []
  (proxy [AbstractTransferListener] []
    (transferSucceeded [^TransferEvent event]
      (let [res    (.getResource event)
            coords (resource->bom-coords (.getResourceName res))]
        (when coords
          (prewarm-log (format "Downloaded BOM %s from %s" coords (.getRepositoryUrl res))))))))

(defn prewarm-import-graph!
  "Breadth-first download of the POM import closure of `seed-artifacts` into the
   local repo, resolving each level's POMs concurrently across `threads` workers.

   `readArtifactDescriptor` resolves a BOM's transitive <scope>import</scope>
   graph one POM at a time, serially, inside Aether's model builder -- for an
   aggregator like com.google.cloud/libraries-bom that is ~220 sequential
   network round-trips. We can't parallelize the model builder, but if every POM
   is already in the local repo that serial walk runs from disk and is fast. So
   we discover the graph ourselves and download each level concurrently.
   Discovery is best-effort: a missed POM just falls back to Aether fetching it
   serially later, and an over-fetched POM is harmless -- Aether still computes
   the authoritative managed-dependency list."
  [^RepositorySystem system ^RepositorySystemSession session repositories seed-artifacts threads]
  (let [pool (Executors/newFixedThreadPool (int threads))
        seen (atom #{})]
    (try
      (loop [frontier (set (map (fn [^Artifact a]
                                  [(.getGroupId a) (.getArtifactId a) (.getVersion a)])
                                seed-artifacts))]
        (let [todo (vec (remove @seen frontier))]
          (when (seq todo)
            (swap! seen into todo)
            (let [tasks (map (fn [[g a v]]
                               (reify Callable
                                 (call [_]
                                   (try
                                     (let [art (DefaultArtifact. ^String g ^String a "" "pom" ^String v)
                                           res (.resolveArtifact system session
                                                                 (ArtifactRequest. art repositories nil))
                                           f   (.. res getArtifact getFile)]
                                       (when f (pom-next-coords f)))
                                     (catch Exception _ nil)))))
                             todo)
                  futs  (.invokeAll pool ^java.util.Collection tasks)
                  next  (into #{} (comp (map #(.get ^Future %)) cat) futs)]
              (recur next)))))
      (finally
        (.shutdown pool)))))

(defn read-artifact-descriptors* [& {:keys [repositories coordinates files retrieve local-repo
                                            transfer-listener offline? proxy mirrors repository-session-fn
                                            prewarm prewarm-threads]
                                     :or   {retrieve        true
                                            prewarm         (Boolean/parseBoolean (System/getProperty "lein-bom.prewarm" "true"))
                                            prewarm-threads (Integer/parseInt (System/getProperty "lein-bom.prewarm.threads" "24"))}}]
  (let [repositories (or repositories maven-central)
        ^RepositorySystem system (repository-system)
        mirror-selector-fn (memoize (partial mirror-selector-fn mirrors))
        mirror-selector (mirror-selector mirror-selector-fn proxy)
        ^RepositorySystemSession session (let [s ((or repository-session-fn
                                                      artifact-descriptors-repository-session)
                                                   {:repository-system system
                                                    :local-repo        local-repo
                                                    :offline?          offline?
                                                    :transfer-listener transfer-listener
                                                    :mirror-selector   mirror-selector})]
                                           ;; log BOM POM downloads, unless the caller wired up its own listener
                                           (when (and prewarm (nil? transfer-listener)
                                                      (instance? DefaultRepositorySystemSession s))
                                             (.setTransferListener ^DefaultRepositorySystemSession s
                                                                   (download-logging-transfer-listener)))
                                           s)
        deps (->> coordinates
                  (map #(if-let [local-file (get files %)]
                          (-> (artifact %)
                              (.setProperties {ArtifactProperties/LOCAL_PATH (.getPath (io/file local-file))}))
                          (artifact %)))
                  vec)
        repositories (vec (map #(let [repo (make-repository % proxy)]
                                  (-> session
                                      (.getMirrorSelector)
                                      (.getMirror repo)
                                      (or repo)))
                               repositories))
        ]
    (when (and prewarm (not offline?))
      (prewarm-import-graph! system session repositories deps prewarm-threads))
    (for [dep deps]
      (.readArtifactDescriptor system session (ArtifactDescriptorRequest. dep repositories nil)))))

(defn read-artifact-descriptors [& args]
  (let [{:keys [coordinates]} (apply hash-map args)]
    (->> (apply read-artifact-descriptors* args)
         (map
           (fn [coord result]
             {:pre [coord result]}
             (let [m (when
                       (instance? ArtifactDescriptorResult result)
                       {:file (.. ^ArtifactDescriptorResult result getArtifact getFile)})]
               (with-meta
                 coord
                 (merge {:result result} m))))
           coordinates))))
