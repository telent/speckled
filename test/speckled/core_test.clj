(ns speckled.core-test
  (:require [clojure.test :refer :all]
            [clj-http.client :as http]
            [speckled.sparql :as sparql :refer [query group ? solve]]
            [speckled.rdf :refer [u]]
            [speckled.core :refer :all]))

;; these are integration tests which run against a real sparql endpoint

(deftest sparql-query
  (let [q (query (comp :body http/post)
                 (solve
                  (group [(? :a)
                          :rdfs:label
                          "Bertrand Russell Peace Foundation"]
                         [(? :a) :rdf:type :dct:Agent]
                         )
                  [(u "http://booksh.lv/bnbgraph")]))]
    (is (some
         #(=
           (u "http://bnb.data.bl.uk/id/agent/BertrandRussellPeaceFoundation")
           (:a %))
         q))))
