(defproject clj-gensim "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [net.mikera/core.matrix "0.57.0" :exclusions [org.clojure/clojure org.clojure/tools.reader]]
                 [net.mikera/vectorz-clj "0.45.0" :exclusions [org.clojure/clojure org.clojure/tools.reader]]
                 [org.apache.lucene/lucene-core "6.3.0"]
                 [org.apache.lucene/lucene-analyzers-common "6.3.0"]])
