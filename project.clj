(defproject fi.lupapiste/lein-bom "0.3.0"
  :description "A leiningen plugin that provides support for importing maven BOMs"
  :url "https://github.com/lupapiste/lein-bom"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.8.1"
  :eval-in :leiningen
  :plugins [[lein-cloverage "1.0.10"]]
  :deploy-repositories [["clojars" {:username      :env/clojars_username
                                    :password      :env/clojars_token
                                    :sign-releases false}]])
