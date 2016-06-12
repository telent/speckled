(ns speckled.dsl
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


;;;; SPARQL query generation

;;;; We expect this ns will generally be used with `:refer :all`, so
;;;; it's important to keep vars private unless they form part of the API


;;; Variables and individual terms

(deftype Variable [name])
(defn ? [n] (->Variable n))
(defmethod rdf/serialize-term Variable [v] (str "?" (name (.name v))))

(deftest ^{:private true} serialize-variable
  (is (= (rdf/serialize-term (? :foo)) "?foo")))

(with-test
  (defn- triple-to-string [triple]
    (if (string? triple)
      triple
      (str/join " " (map rdf/serialize-term triple))))

  (binding [rdf-base-uri "http://f.com/"]
    (is (= (triple-to-string [(u "a")
                              (u "b")
                              (u "c")])
           "<http://f.com/a> <http://f.com/b> <http://f.com/c>")))
  (is (= (triple-to-string
          "<http://f.com/a> <http://f.com/b> <http://f.com/c>")
         "<http://f.com/a> <http://f.com/b> <http://f.com/c>" )))

(defmulti ^{:private true} to-string-fragment class)

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

(deftest ^{:private true} rdf-formatting
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

(defn- collapse-whitespace [s]
  (str/trim (str/replace s #"\s+" " ")))
(defn- equal-but-for-whitespace [a b]
  (= (collapse-whitespace a) (collapse-whitespace b)))

(defn- delete-prefixes [str]
  "For test assertions: strip PREFIX and BASE directives from string"
  (str/replace str #"(?m)^(PREFIX|BASE).*\n+"  ""))

(defn- equal-form
  "For test assertions: compare arguments after removing PREFIX and BASE directives from each"
  [a b]
  (equal-but-for-whitespace (delete-prefixes a) (delete-prefixes b)))


(deftest ^{:private true} union-city-blues
  (binding [rdf-base-uri "http://f.com/"]
    (let [u (union (group [(u "a") (u "b") (u "c")])
                   (group [(? :s) :rdf:a "foo"]))]
      (is (= (collapse-whitespace (to-string-fragment u))
             "{ { <http://f.com/a> <http://f.com/b> <http://f.com/c>} UNION { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} }")))))

(deftype Binding [variable value group])
(derive Binding ::graph)
(defn bind [[variable value] group]
  (->Binding variable value group))

(defmethod to-string-fragment Binding [b]
  (str (to-string-fragment (.group b))
       " BIND("
       (rdf/serialize-term (.value b))
       " AS " (rdf/serialize-term (.variable b))
       ")\n"))

(deftest ^{:private true} bind-us-together
  (binding [rdf-base-uri "http://f.com/"]
    (let [b (bind [(? :foo ) "19281"]
                  (group [(? :s) :rdf:a "foo"]))]
      (is (= (collapse-whitespace (to-string-fragment b))
             "{ ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} BIND(\"19281\" AS ?foo)")))))


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

(defmulti ^{:private true} rdf-to-soln-seq class)

(defmethod rdf-to-soln-seq ::soln-seq [s] s)
(defmethod rdf-to-soln-seq ::graph [s] (solve s))
(defmethod rdf-to-soln-seq String [s] s)


(deftest ^{:private true} rdf-solve
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

(deftype Projection [variables soln-seq])
(derive ::modified-soln-seq ::soln-seq)
(derive Projection ::soln-seq)

(defmethod to-string-fragment Projection [v]
  (let [variables (if (= (.variables v) :*)
                    "*"
                    (map rdf/serialize-term (.variables v)))]
    (str "SELECT " (str/join " " variables) " "
         (to-string-fragment (.soln-seq v)))))

(defn project
  ([v s] (->Projection v (rdf-to-soln-seq s)))
  ([s] (->Projection :* (rdf-to-soln-seq s))))

(deftest ^{:private true} projection-test
  (is (equal-but-for-whitespace
       (to-string-fragment
        (project (map ? [:n :ed :title])
                 (solve
                  (group [(? :n) :foaf:copyOf (? :ed)]
                         [(? :n) :rdfs:label (? :title)]))))
       "SELECT ?n ?ed ?title WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))
    (is (equal-but-for-whitespace
       (to-string-fragment
        (project (map ? [:n :ed :title])
                 ;; implicit solve
                 (group [(? :n) :foaf:copyOf (? :ed)]
                        [(? :n) :rdfs:label (? :title)])))
       "SELECT ?n ?ed ?title WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n")))


(deftype Limit [soln-seq limit])
(derive Limit ::modified-soln-seq)

(defn limit [projection max]
  (->Limit (rdf-to-soln-seq projection) max))

(defmethod to-string-fragment Limit [v]
  (str (to-string-fragment (.soln-seq v))
       " LIMIT " (.limit v)))

(deftest ^{:private true} limits
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

(derive ::query-form ::top-level-form)
(derive ::update-form ::top-level-form)

(defmulti ^{:private true} rdf-to-top-level-form class)
(defmethod rdf-to-top-level-form ::top-level-form [s] s)
(defmethod rdf-to-top-level-form String [s] s)

(deftype Select [solution-seq])
(derive Select ::query-form)

(with-test
  (defn- has-projection? [soln-seq]
    (let [c (class soln-seq)]
      (or (isa? c Projection)
          (and (isa? c ::modified-soln-seq)
               (has-projection? (.soln-seq soln-seq))))))
  (is (has-projection? (project [] (solve [[:a :b :c]]))))
  (is (has-projection? (limit (project [] (solve [[:a :b :c]])) 5)))
  (is (not (has-projection? (solve [[:a :b :c]])))))

(defmethod to-string-fragment Select [v]
  ;; do not add select * if the solution-seq already contains a projection
  (let [s (.solution-seq v)]
    (if (has-projection? s)
      (to-string-fragment (.solution-seq v))
      (str "SELECT * " (to-string-fragment (.solution-seq v))))))

(defn select [soln-seq] (->Select soln-seq))

(defmethod rdf-to-top-level-form ::soln-seq [s] (select s))
(defmethod rdf-to-top-level-form ::graph [s] (select (rdf-to-soln-seq s)))

(deftype Construction [template solution-seq])
(derive Construction ::query-form)

(defmethod to-string-fragment Construction [v]
  (str "CONSTRUCT " (to-string-fragment (.template v))
       (to-string-fragment (.solution-seq v))))

(defn construct [template soln-seq] (->Construction template soln-seq))

(deftest ^{:private true} rdf-construction
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

(deftype Insert [graph solution-seq])
(derive Insert ::update-form)

(defmethod to-string-fragment Insert [v]
  (if-let [s (.solution-seq v)]
    (str "INSERT "
         (to-string-fragment (.graph v))
         (to-string-fragment (rdf-to-soln-seq s)))
    (str "INSERT DATA "
         (to-string-fragment (.graph v)))))

(defn insert
  ([graph soln-seq] (->Insert graph soln-seq))
  ([graph] (->Insert graph nil)))

;; prepend prefix declarations

(defn- declare-prefixes [query]
  (let [ns-string (str/join "\n"
                            (map (fn [[k v]] (str "PREFIX " (name k) ": "
                                                  (rdf/serialize-term (u v))))
                                 rdf/prefixes))]
    (str ns-string "\n"
         "BASE " (rdf/serialize-term (u rdf/rdf-base-uri)) "\n\n"
         query)))

(defn ->string [sparql]
  (-> sparql
      rdf-to-top-level-form
      to-string-fragment
      declare-prefixes))

(deftest ^{:private true} select-test
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

(deftest ^{:private true} project-and-select-test
  (let [s (->string
           (select
            (project
             [(? :a)]
             (group [(? :a)
                     :rdfs:label
                     "Bertrand Russell Peace Foundation"]
                    [(? :a) :rdf:type :dct:Agent]
                    ))))]
    (is (.contains s "PREFIX rdf:"))
    (is (equal-but-for-whitespace
         (delete-prefixes s)
         "SELECT ?a WHERE
{
 ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
 ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}\n"))))


(deftest ^{:private true} implicit-select-test
  (let [s (->string
           (group [(? :a)
                   :rdfs:label
                   "Bertrand Russell Peace Foundation"]
                  [(? :a) :rdf:type :dct:Agent]
                  ))]
    (is (.contains s "PREFIX rdf:"))
    (is (equal-but-for-whitespace
         (delete-prefixes s)
         "SELECT * WHERE
{
 ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
 ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}\n"))))

(deftest ^{:private true} insert-test
  (is
   (equal-form
    (->string
     (insert
      (group [(URI. "http://example.com") :foaf:nick "granddad"])
      (solve (group [(? :a) :rdfs:label (? :b)]))))
    "INSERT { \n<http://example.com> <http://xmlns.com/foaf/0.1/nick> \"granddad\"} WHERE  { ?a <http://www.w3.org/2000/01/rdf-schema#label> ?b} "))
  (is
   (equal-form
    (->string
     (insert
      (group [(URI. "http://example.com") :foaf:nick "granddad"])))
    "INSERT DATA { \n<http://example.com> <http://xmlns.com/foaf/0.1/nick> \"granddad\"}")))

(deftest ^{:private true} test-construct-query
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