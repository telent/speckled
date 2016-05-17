(ns speckled.sparql
  (:import [org.apache.commons.codec.net URLCodec]
           [java.text SimpleDateFormat]
           [java.net URL URI])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.test :as test :refer [is deftest with-test]]
            [speckled.rdf :as rdf :refer [u rdf-base-uri]])
  (:use [clojure.data.zip.xml :only (attr text xml-> xml1-> )]))

;;; sparql network code
;;; sparql query construction
;;; SPARQL Query Results XML reader


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
  (first (:content (first el))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sparql query generation

;; variables and individual terms


(deftype Variable [name])
(defn ? [n] (->Variable n))
(defmethod rdf/serialize-term Variable [v] (str "?" (name (.name v))))

(deftest serialize-variable
  (is (= (rdf/serialize-term (? :foo)) "?foo")))

(defn triple-to-string [triple]
  (if (string? triple)
    triple
    (str/join " " (map rdf/serialize-term triple))))

(deftest triple-to-string-test
  (binding [rdf-base-uri "http://f.com/"]
    (is (= (triple-to-string [(u "a")
                              (u "b")
                              (u "c")])
           "<http://f.com/a> <http://f.com/b> <http://f.com/c>")))
  (is (= (triple-to-string
          "<http://f.com/a> <http://f.com/b> <http://f.com/c>")
         "<http://f.com/a> <http://f.com/b> <http://f.com/c>" )))

(defmulti to-string-fragment class)

;; groups

(deftype Group [triples])
(derive Group ::graph)
(defn group [ & triples ] (->Group triples))
(defmethod to-string-fragment Group [group]
  (str "{\n" (str/join " .\n" (map to-string-fragment (.triples group)))
       "}\n"))

(defmethod to-string-fragment String [s] s)

(defmethod to-string-fragment (class []) [v]
  (triple-to-string v))

(deftest rdf-formatting
  (binding [rdf-base-uri "http://f.com/"
            rdf/prefixes (assoc rdf/prefixes "shlv" "http://booksh.lv/ns#")]
    (is (= (to-string-fragment "{ ?n ?v ?o }") "{ ?n ?v ?o }"))
    (let [g (group [(u "a") (u "b") (u "c")])]
      (is (= (to-string-fragment g)
             "{\n<http://f.com/a> <http://f.com/b> <http://f.com/c>}\n")))
    (let [g2 (group [(u "a") (u "b") (u "c")]
                    [:shlv:a :shlv:b (? :done)])]
      (is (= (to-string-fragment g2)
             "{\n<http://f.com/a> <http://f.com/b> <http://f.com/c> .\n<http://booksh.lv/ns#a> <http://booksh.lv/ns#b> ?done}\n")))))

(deftype Union [groups])
(derive Union ::graph)
(defn union [ & groups ] (->Union groups))
(defmethod to-string-fragment Union [u]
  (str "{ "
       (str/join "\n    UNION\n" (map to-string-fragment (.groups u)))
       " }"))

(defn collapse-whitespace [s]
  (str/trim (str/replace s #"\s+" " ")))
(defn equal-but-for-whitespace [a b]
  (= (collapse-whitespace a) (collapse-whitespace b)))

(defn delete-prefixes [str]
  "For test assertions: strip PREFIX and BASE directives from string"
  (str/replace str #"(?m)^(PREFIX|BASE).*\n+"  ""))

(defn equal-form
  "For test assertions: compare arguments after removing PREFIX and BASE directives from each"
  [a b]
  (equal-but-for-whitespace (delete-prefixes a) (delete-prefixes b)))


(deftest union-city-blues
  (binding [rdf-base-uri "http://f.com/"]
    (let [u (union (group [(u "a") (u "b") (u "c")])
                   (group [(? :s) :rdf:a "foo"]))]
      (is (= (collapse-whitespace (to-string-fragment u))
             "{ { <http://f.com/a> <http://f.com/b> <http://f.com/c>} UNION { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} }")))))

;; solve to find the values of variables appearing in a group
;;  => get solution sequence

(deftype SolutionSequence [group default-graphs named-graphs])
(derive SolutionSequence ::soln-seq)

(defmethod to-string-fragment SolutionSequence [s]
  (str
   (str/join
    (map (fn [iri] (str " FROM " (rdf/serialize-term iri) "\n"))
         (.default-graphs s)))
   (str/join
    (map (fn [iri] (str " FROM NAMED " (rdf/serialize-term iri) "\n"))
         (.named-graphs s)))
   " WHERE "
   (to-string-fragment (.group s))))


(defn solve [group & [default-graphs named-graphs]]
  (->SolutionSequence group default-graphs named-graphs))


(deftest rdf-solve
  (let [g (group [(? :n) :foaf:copyOf (? :ed)]
                 [(? :n) :rdfs:label (? :title)])]
    (is (equal-but-for-whitespace
         (to-string-fragment (solve g))
         "WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))
    (is (equal-but-for-whitespace
         (to-string-fragment
          (solve g [(u "http://booksh.lv/bnbgraph")]))
         "FROM <http://booksh.lv/bnbgraph>\n WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"
         ))))

;; Operations on solution sequences to filter/rearrange/transform them

(deftype Projection [variables group])
(derive Projection ::soln-seq)

(defmethod to-string-fragment Projection [v]
  (let [variables (if (= (.variables v) :*)
                    "*"
                    (map rdf/serialize-term (.variables v)))]
    (str "SELECT " (str/join " " variables)
         (to-string-fragment (.group v)))))

(defn project
  ([v g] (->Projection v g))
  ([g] (->Projection :* g)))

(deftest rdf-projection
  (is (= (to-string-fragment
          (project (map ? [:n :ed :title])
                  (solve
                   (group [(? :n) :foaf:copyOf (? :ed)]
                          [(? :n) :rdfs:label (? :title)]))))
         "SELECT ?n ?ed ?title WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n")))

(defmulti rdf-to-soln-seq class)

(defmethod rdf-to-soln-seq ::soln-seq [s] s)
(defmethod rdf-to-soln-seq ::graph [s] (solve s))
(defmethod rdf-to-soln-seq String [s] s)

(deftype Limit [proj limit])
(derive Limit ::soln-seq)

(defn limit [projection max]
  (->Limit (rdf-to-soln-seq projection) max))

(defmethod to-string-fragment Limit [v]
  (str (to-string-fragment (.proj v))
       " LIMIT " (.limit v)))

(deftest limits
  (is (equal-but-for-whitespace
       (to-string-fragment
        (limit
         (project (map ? [:a :b])
                 (solve (group [(? :a) :rdfs:label (? :b)])))
         3))
       "SELECT ?a ?b WHERE { ?a <http://www.w3.org/2000/01/rdf-schema#label> ?b} LIMIT 3")))

;; Top-level forms: given a (possibly modified) solution sequence,
;; create an entire sparql statement:
;;  ASK - find out whether there were solutions
;;  SELECT - tell me the matching values of the variables
;;  CONSTRUCT - return some new triples using the variables
;;  INSERT - modify the datastore to add new triples using the variables

(defmulti rdf-to-query-form class)
(defmethod rdf-to-query-form ::query-form [s] s)
(defmethod rdf-to-query-form ::soln-seq [s] (project s))
(defmethod rdf-to-query-form ::graph [s] (project (rdf-to-soln-seq s)))
(defmethod rdf-to-query-form String [s] s)

(deftype Select [solution-seq])
(derive Select ::query-form)

(defn has-projection? [expr]
  false)

(defmethod to-string-fragment Select [v]
  ;; do not add select * if the solution-seq already contains a projection
  (let [s (.solution-seq v)]
    (if (has-projection? s)
      (to-string-fragment (.solution-seq v))
      (str "SELECT * " (to-string-fragment (.solution-seq v))))))

(defn select [soln-seq] (->Select soln-seq))



(deftype Construction [template solution-seq])
(derive Construction ::query-form)

(defmethod to-string-fragment Construction [v]
  (str "CONSTRUCT " (to-string-fragment (.template v))
       (to-string-fragment (.solution-seq v))))

(defn construct [template soln-seq] (->Construction template soln-seq))

(deftest rdf-construction
  (binding [rdf/prefixes (assoc rdf/prefixes "shlv" "http://booksh.lv/ns#")]
    (is (equal-but-for-whitespace
         (to-string-fragment
          (construct (group [(? :n) :shlv:isA :shlv:book]
                            [(? :n) :shlv:edition (? :ed)]
                            [(? :n) :shlv:title (? :title)])
                     (solve
                      (group [(? :n) :shlv:copyOf (? :ed)]
                             [(? :n) :rdfs:label (? :title)]))))
         "CONSTRUCT {\n?n <http://booksh.lv/ns#isA> <http://booksh.lv/ns#book> .\n?n <http://booksh.lv/ns#edition> ?ed .\n?n <http://booksh.lv/ns#title> ?title}\n WHERE {\n?n <http://booksh.lv/ns#copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))))

;; maybe we could turn this into an Update with a ::update-form kind
(defn triples-to-string [triples]
  (str
   "INSERT DATA { \n"
   (str/join " . \n"
             (map triple-to-string triples))
   "\n}"))

(deftest triples-to-string-test
  (is
   (equal-but-for-whitespace
    (triples-to-string
     [[(URI. "http://example.com") :foaf:nick "granddad"]])
    "INSERT DATA { \n<http://example.com> <http://xmlns.com/foaf/0.1/nick> \"granddad\"\n}")))

(defn declare-prefixes [query]
  (let [ns-string (str/join "\n"
                            (map (fn [[k v]] (str "PREFIX " (name k) ": "
                                                  (rdf/serialize-term (u v))))
                                 rdf/prefixes))]
    (str ns-string "\n"
         "BASE " (rdf/serialize-term (u rdf/rdf-base-uri)) "\n\n"
         query)))

(with-test
  (defn ->string [sparql]
    (-> sparql
        rdf-to-query-form
        to-string-fragment
        declare-prefixes))
  (let [s (->string
           (group [(? :a)
                   :rdfs:label
                   "Bertrand Russell Peace Foundation"]
                  [(? :a) :rdf:type :dct:Agent]
                  ))]
    (is (.contains s "PREFIX rdf:"))
    (is (equal-but-for-whitespace
         (delete-prefixes s)
         "SELECT *
WHERE {
 ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
 ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}\n"))))

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

#_
(defn insert-triples [triples]
  (update-store (triples-to-string triples)))

#_(defn random-string []
  "JKHgjkhgkjH")
#_
(defmacro with-test-rdf-root [ & body]
  (let [prefix (str "http://test.booksh.lv/res/" (random-string) "/")]
    `(binding [rdf-base-uri ~prefix]
       (try
         ~@body
         (finally
           (update-store
            (str "DELETE { ?n ?v ?o } where { "
                 "?n ?v ?o . "
                 "filter(regex(str(?n), \"^http://test.booksh.lv/\") || "
                 "       regex(str(?o), \"^http://test.booksh.lv/\")) . "
                 "}")))))))
#_
(deftest test-data-test
  ;; insert some test triples, test they're present
  (with-fixture-db
    (with-test-rdf-root
      (let [s (u "shelved/1")
            triples
            [[:bnb:017048941 :shlv:shelvedEvent s]
             [s :shlv:library (u "libraries/10")]
             [s :shlv:shelf "top shelf living room"]
             [s :shlv:shelvedAt (java.util.Date.)]]]
        (insert-triples triples)
        (let [q (str "select ?v ?o FROM <urn:x-arq:DefaultGraph> FROM <http://booksh.lv/bnbgraph> { " (rdf/serialize-term s) " ?v ?o . }")
              during (query q)]
          (is (= 3 (count during))))))
    (is (empty?
         (query  "SELECT ?n ?v ?o { ?n ?v ?o . filter(regex(str(?n), \"^http://test.booksh.lv/\") || regex(str(?n), \"^http://test.booksh.lv/\")) } LIMIT 2")))))


(defn query-for-graph [post-fn sparql]
  (let [response (post-sparql
                  post-fn
                  (->string sparql)
                  "query"
                  "application/sparql-query"
                  "application/n-triples")]
    (rdf/parse-n-triples response)))


(deftest test-construct-query
  (let [sparql (construct
                (group [(? :n) :rdfs:label "hey"]
                       [(? :n) :foaf:familyName (? :name)])
                (limit
                 (solve (group [(? :n) (? :v) (? :name)]))
                 3))]
    (is (equal-but-for-whitespace
         (delete-prefixes (->string sparql))
         "CONSTRUCT {
   ?n <http://www.w3.org/2000/01/rdf-schema#label> \"hey\" .
   ?n <http://xmlns.com/foaf/0.1/familyName> ?name}
WHERE { ?n ?v ?name} LIMIT 3"
         ))))
