(defproject telent/speckled "0.2.0-SNAPSHOT"
  :description "SPARQL DSL and stuff"
  :url "http://github.com/telent/speckled"
  :repositories [["snapshots" {:url  "https://clojars.org/repo"
                               :username "telent"
                               :password :env/CLOJARS_PASSWORD}]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-RC1"]
                 [org.apache.httpcomponents/httpclient "4.5.1"]
                 [clj-time "0.11.0"]
                 [instaparse "1.4.1"]
                 [org.clojure/data.zip "0.1.1"]
                 ]
  :profiles {
             :test {:dependencies [[clj-http "2.1.0"]]}
             :dev {:dependencies [[clj-http "2.1.0"]]}}

  )
