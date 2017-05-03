(defproject telent/speckled "0.1.0-SNAPSHOT"
  :description "SPARQL DSL and stuff"
  :url "http://example.com/FIXME"
  :local-repo ~(or
                (System/getenv "M2REPOSITORY")
                (.toString (clojure.java.io/file (System/getenv "HOME")
                                                 ".m2/repository")))
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.apache.httpcomponents/httpclient "4.5.1"]
                 [clj-time "0.11.0"]
                 [instaparse "1.4.1"]
                 [org.clojure/data.zip "0.1.1"]
                 [prismatic/schema "1.0.5"]
                 ]
  :profiles {:repl {:plugins [[cider/cider-nrepl "0.12.0"]]}
             :test {:dependencies [[clj-http "2.1.0"]]}
             :dev {:dependencies [[clj-http "2.1.0"]]}}

  )
