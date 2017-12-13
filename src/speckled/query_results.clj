(ns speckled.query-results
  (:import [org.apache.commons.codec.net URLCodec]
           [java.net URI])
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.test :as test :refer [is deftest with-test]]
            [speckled.rdf :as rdf :refer [u]]))

;; parsing the "SPARQL Query Results XML Format"

;; originally reverse-engineered from looking at Fuseki responses, but
;; should correspond with
;; https://www.w3.org/TR/rdf-sparql-XMLres/


(defmulti xml-extract-content (fn [el] (:tag (first el))))

(defmethod xml-extract-content :literal [el]
  (let [datatype (or (-> el first :attrs :datatype)
                     (rdf/xsd "string"))]
    (rdf/make-literal (first (:content (first el))) (u datatype))))

(defmethod xml-extract-content :uri [el]
  (URI. (first (:content (first el)))))

(deftest xml-fishing
  (let [u
        [{:tag :uri, :attrs nil, :content ["http://bnb.data.bl.uk/id/agent/1000LivesImprovements"]}]
        o
        [{:tag :literal, :attrs {:datatype "http://www.w3.org/2001/XMLSchema#string"}, :content ["1000 Lives Improvements"]}]]
    (is (= (xml-extract-content u)
           (URI. "http://bnb.data.bl.uk/id/agent/1000LivesImprovements")))
    (is (= (xml-extract-content o) "1000 Lives Improvements"))))


(defn get-tag [tagname xml]
  (:content (some #(if (= (:tag %) tagname) %) xml)))

(defn get-tags [tagname xml]
  (map :content (filter #(= (:tag %) tagname) xml)))


(defn parse-tree [doc]
  {:pre [(= (:tag doc) :sparql)]}
  (let [results
        (get-tags :result
                  (get-tag :results (:content doc)))]
    (map
     #(into {}
            (map (fn [b]
                   (assert (= (:tag b) :binding))
                   [(keyword (:name (:attrs b)))
                    (xml-extract-content (:content b))]))
            %)
     results)))


(deftest parse-tree-test
  (let [doc
        {:tag :sparql,
         :attrs {:xmlns "http://www.w3.org/2005/sparql-results#"},
         :content
         [
          {:tag :head, :attrs nil, :content [{:tag :variable, :attrs {:name "n"}, :content nil} {:tag :variable, :attrs {:name "v"}, :content nil} {:tag :variable, :attrs {:name "o"}, :content nil}]}
          {:tag :results, :attrs nil, :content
           [
            {:tag :result,
             :attrs nil,
             :content
             [{:tag :binding,
               :attrs {:name "n"},
               :content
               [{:tag :uri,
                 :attrs nil,
                 :content
                 ["http://bnb.data.bl.uk/id/agent/1000LivesImprovements"]}]}
              {:tag :binding,
               :attrs {:name "v"},
               :content
               [{:tag :uri,
                 :attrs nil,
                 :content ["http://www.w3.org/2000/01/rdf-schema#label"]}]}
              {:tag :binding,
               :attrs {:name "o"},
               :content
               [{:tag :literal,
                 :attrs {:datatype "http://www.w3.org/2001/XMLSchema#string"},
                 :content ["1000 Lives Improvements"]}]}]}
            {:tag :result, :attrs nil, :content [{:tag :binding, :attrs {:name "n"}, :content [{:tag :uri, :attrs nil, :content ["http://bnb.data.bl.uk/id/agent/1000LivesImprovements"]}]} {:tag :binding, :attrs {:name "v"}, :content [{:tag :uri, :attrs nil, :content ["http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]}]} {:tag :binding, :attrs {:name "o"}, :content [{:tag :uri, :attrs nil, :content ["http://purl.org/dc/terms/Agent"]}]}]}
            {:tag :result, :attrs nil, :content [{:tag :binding, :attrs {:name "n"}, :content [{:tag :uri, :attrs nil, :content ["http://bnb.data.bl.uk/id/agent/1000LivesImprovements"]}]} {:tag :binding, :attrs {:name "v"}, :content [{:tag :uri, :attrs nil, :content ["http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]}]} {:tag :binding, :attrs {:name "o"}, :content [{:tag :uri, :attrs nil, :content ["http://xmlns.com/foaf/0.1/Agent"]}]}]}
            ]}
          ]}
        results
        [{:n
          (URI. "http://bnb.data.bl.uk/id/agent/1000LivesImprovements")
          :v
          (URI. "http://www.w3.org/2000/01/rdf-schema#label")
          :o "1000 Lives Improvements"}
         {:n
          (URI. "http://bnb.data.bl.uk/id/agent/1000LivesImprovements")
          :v
          (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          :o
          (URI. "http://purl.org/dc/terms/Agent")}
         {:n
          (URI."http://bnb.data.bl.uk/id/agent/1000LivesImprovements")
          :v
          (URI. "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          :o
          (URI. "http://xmlns.com/foaf/0.1/Agent")}]]
    (is (= (parse-tree doc)
           results))))

(defn read-query-results [reader]
  (parse-tree (xml/parse reader)))




;; 171379



(comment

  (let [post-rq #(http/post "http://localhost:3030/agraph/query"
                            {:content-type "application/sparql-query"
                             :body %
                             :accept "application/n-triples"})]
    (-> my-query-form
        ->string
        post-rq
        deref :body
        read-query-results)))
