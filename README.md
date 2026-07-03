# lein-bom

A Leiningen plugin that provides support for importing Maven "Bill Of Materials" (BOM) dependencies with parallel
breadth-first download of transitive POMs, useful for very large BOMs.

[![Clojars Project](https://clojars.org/fi.lupapiste/lein-bom/latest-version.svg)](https://clojars.org/fi.lupapiste/lein-bom)

Forked from https://github.com/tsachev/lein-bom.

## Install

Put `[fi.lupapiste/lein-bom "0.3.0"]` into the `:plugins` vector of your `project.clj`.

## Usage

Specify bom dependencies using `:import` vector in `:bom` map of your `project.clj`.

```clojure
:bom {:import [[com.fasterxml.jackson/jackson-bom "2.21.4"]]}
```

To see actual managed dependencies, run:

```
$ lein bom
```

## License

Original Copyright © 2020 Vladimir Tsanev

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
