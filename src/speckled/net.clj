(ns speckled.net
  (:import [org.apache.commons.codec.net URLCodec]
           [java.text SimpleDateFormat]
           [java.net URL URI])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.test :as test :refer [is deftest with-test]]
            [speckled.rdf :as rdf :refer [u ]]
            [speckled.dsl :as dsl :refer [->string]])
  (:use [clojure.data.zip.xml :only (attr text xml-> xml1-> )]))

;;; sparql network code

(def ^:dynamic fuseki-service-url "http://localhost:3030/small/")

(defmacro with-fixture-db [& body]
  `(binding [fuseki-service-url "http://localhost:3030/scratch/"]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing query results (xml)

(defn ->zipper [string]
  (with-open [in (java.io.ByteArrayInputStream. (.getBytes (.trim string)))]
    (try
      (zip/xml-zip (xml/parse in))
      (catch org.xml.sax.SAXParseException e
        (prn (str "Can't parse " string))
        {}))))

(defmulti xml-extract-content (fn [el] (:tag (first el))))

(defmethod xml-extract-content :literal [el]
  (let [datatype (or (-> el first :attrs :datatype)
                     :xsd:string)]
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


(defn bindings-to-map [b]
  (let [for-each (fn [m {:keys [tag attrs content]}]
                   (assoc m
                          (keyword (:name attrs))
                          (xml-extract-content content)))]
    (reduce for-each {} b)))

(defn parse-sparql-document [doc]
  {:pre [(= (:tag (first doc)) :sparql)]}
  (let [results
        (get-tags :result
                  (get-tag :results (get-tag :sparql doc)))]
    (map
     #(into {}
            (map (fn [b]
                   (assert (= (:tag b) :binding))
                   [(keyword (:name (:attrs b)))
                    (xml-extract-content (:content b))]))
            %)
     results)))


(deftest parse-sparql-document-test
  (let [doc
        [{:tag :sparql,
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
           ]} nil]
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
    (is (= (parse-sparql-document doc)
           results))))

(defn parse-sparql-response-body [body]
  (parse-sparql-document (->zipper body)))


(defn post-sparql [post-fn payload uri content-type accept]
  (post-fn (str fuseki-service-url uri)
           {:content-type content-type
            :accept accept
            :body payload}))

(defn query [post-fun sparql]
  (let [body (post-sparql post-fun
                          (->string sparql)
                          "query"
                          "application/sparql-query"
                          "application/sparql-results+xml")]
    (parse-sparql-response-body body)))

(defn update-store [post-fun sparql]
  ;; according to
  ;; https://www.w3.org/TR/2013/REC-sparql11-protocol-20130321/#update-operation
  ;; "The response body of a successful update request is
  ;; implementation defined."
  ;; so if we get to the end of post-sparql, we kind of have to
  ;; assume everything worked
  (and
   (post-sparql post-fun
                (->string sparql)
                "update"
                "application/sparql-update"
                "text/*")
   true))

(defn query-for-graph [post-fn sparql]
  (let [response (post-sparql
                  post-fn
                  (->string sparql)
                  "query"
                  "application/sparql-query"
                  "application/n-triples")]
    (rdf/parse-n-triples response)))
