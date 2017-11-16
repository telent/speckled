;;;; SPARQL query generation

;;;; We expect this ns will generally be used with `:refer :all`, so
;;;; it's important to keep vars private unless they form part of the API

(ns speckled.dsl
  (:import [org.apache.commons.codec.net URLCodec]
           [java.text SimpleDateFormat]
           [java.net URL URI])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test :refer [is deftest with-test]]
            [speckled.rdf :as rdf :refer [u]]))


;;; The rdf namespace deals only with n-triples, which does not support
;;; prefixed names.  But we can extend it ...

(defrecord PrefixedName [prefix name])

(defmethod rdf/u clojure.lang.Keyword [k]
  (PrefixedName. (namespace k) (name k)))

(def ^:dynamic prefixes
  {"dct" "http://purl.org/dc/terms/"
   "foaf" "http://xmlns.com/foaf/0.1/"
   "owl" "http://www.w3.org/2002/07/owl#"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
   "xsd" "http://www.w3.org/2001/XMLSchema#"
   "void" "http://rdfs.org/ns/void#"})

(defmethod rdf/serialize-term PrefixedName [n]
  (let [prefix (:prefix n)]
    (rdf/serialize-term (u (str (get prefixes prefix) (:name n))))))

(deftest serialize-prefixed-name-test
  (is (= (rdf/serialize-term (u :rdf/type))
         "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")))


;;; Variables and individual terms

(deftype Variable [name])
(defn ? [n] (->Variable n))
(defmethod rdf/serialize-term Variable [v] (str "?" (name (.name v))))

(deftest ^{:private true} serialize-variable
  (is (= (rdf/serialize-term (? :foo)) "?foo")))


;;; almost everything past this point responds to to-string-fragment

(defmulti ^{:private true} to-string-fragment class)

(defmethod to-string-fragment String [s] s)


;;; expressions (for filter, bind, and rename operations)

(deftype Expr [term])

(def inline-operators (set (map name '(+ - * / = < > <= >=))))

(defn stringize-expr [term]
  (cond
    (seq? term)
    (let [[op & args] term
          op (name op)]
      (if (contains? inline-operators op)
        ;; (+ a b c d) => "((((a) + b) + c) + d)"
        (reduce (fn [e r]
                  (if e
                    (str "(" e " " op " " r ")")
                    (str "(" r ")")))
                (map stringize-expr args))
        (str op "(" (str/join ", " (map stringize-expr args)) ")")))

    ;; not at all sure it is sensible to support this syntax, need to think
    ;; about it some more.  But if we don't, user needs to antiquote
    ;; variables in quoted expressions :-(
    (and (symbol? term) (= (first (name term)) \?))
    (name term)

    :else
    (rdf/serialize-term term)))

(deftest ^{:private true} express-yourself
  (is (= (stringize-expr '7) "7"))
  (is (= (stringize-expr '?foo) "?foo"))

  (is (= (stringize-expr (u "http://f.com")) "<http://f.com>"))
  (is (= (stringize-expr :rdf/type) "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"))

  ;; variadic inline op
  (is (= (stringize-expr '(+ 1 2 3)) "((1 + 2) + 3)"))
  (is (= (stringize-expr '(+ 1 2 3 17)) "(((1 + 2) + 3) + 17)"))

  ;; nested operations
  (is (= (stringize-expr '(+ 1 (- 2 3) 4)) "((1 + (2 - 3)) + 4)"))

  ;; function call
  (is (= (stringize-expr '(fn 6 7 8)) "fn(6, 7, 8)"))
  (is (= (stringize-expr '(fn 6 (+ 7 7) 8)) "fn(6, (7 + 7), 8)"))
  )

(defmethod to-string-fragment Expr [e]
  (stringize-expr (.term e)))


;;; triples

;;; supporting rdf list syntax was in the "no, why would you ever
;;; need *that*?" category right up until I found that jena property
;;; functions (notably, fulltext search on lucene indices) are invoked
;;; this way

(defmethod to-string-fragment (class []) [triple]
  (let [element-to-string
        (fn [e]
          (if (list? e)
            (str "(" (str/join " " (map rdf/serialize-term e)) ")")
            (rdf/serialize-term e)))]
    (str/join " " (map element-to-string triple))))

(deftest ^{:private true} triple-tested
  (is (= (to-string-fragment [(u "http://f.com/a")
                              (u "http://f.com/b")
                              (u "http://f.com/c")])
         "<http://f.com/a> <http://f.com/b> <http://f.com/c>"))
  (is (= (to-string-fragment [(u "http://f.com/a")
                              :rdf/fulltext
                              (list :rdfs/label "term" 25)])
         "<http://f.com/a> <http://www.w3.org/1999/02/22-rdf-syntax-ns#fulltext> (<http://www.w3.org/2000/01/rdf-schema#label> \"term\" 25)"
         )))

;; groups

(deftype Group [triples])
(derive Group ::graph)
(defn group [ & triples ] (->Group triples))
(defmethod to-string-fragment Group [group]
  (str "{\n" (str/join " .\n" (map to-string-fragment (.triples group)))
       "}\n"))

(deftest ^{:private true} rdf-formatting
  (binding [prefixes (assoc prefixes "ex" "http://example.com/ns#")]
    (is (= (to-string-fragment "{ ?n ?v ?o }") "{ ?n ?v ?o }"))
    (let [a (u "http://f.com/a")
          b (u "http://f.com/b")
          c (u "http://f.com/c")]
      (is (= (to-string-fragment (group [a b c]))
             "{\n<http://f.com/a> <http://f.com/b> <http://f.com/c>}\n"))
      (let [g2 (group [a b c]
                      [:ex/a :ex/b (? :done)])]
        (is (= (to-string-fragment g2)
               "{\n<http://f.com/a> <http://f.com/b> <http://f.com/c> .\n<http://example.com/ns#a> <http://example.com/ns#b> ?done}\n"))))))


(deftype NamedGraph [graphname group])
(derive NamedGraph ::graph)
(defn with-graph [graphname group] (->NamedGraph graphname group))
(defmethod to-string-fragment NamedGraph [ng]
  (str "{ GRAPH " (rdf/serialize-term (.graphname ng))
       "\n" (to-string-fragment (.group ng))
       "} "))

(deftype FilteredGraph [graph expr])
(derive FilteredGraph ::graph)
(defn filter-solns [graph expr] (->FilteredGraph graph (->Expr expr)))
(defmethod to-string-fragment FilteredGraph [g]
  (str "{\n"
       (to-string-fragment (.graph g))
       " FILTER (" (to-string-fragment (.expr g)) ")\n"
       "}\n"))

(deftest ^{:private true} love-philtre
  (let [g (filter-solns (group [(? :a) :foaf/weblog (? :blog)])
                        '(contains ?blog "livejournal"))]
    (is (= (to-string-fragment  g)
           "{\n{\n?a <http://xmlns.com/foaf/0.1/weblog> ?blog}\n FILTER (contains(?blog, \"livejournal\"))\n}\n"))))


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
  (let [u (union (group [(u "http://f.com/a") (u "http://f.com/b") (u "http://f.com/c")])
                 (group [(? :s) :rdf/a "foo"]))]
    (is (= (collapse-whitespace (to-string-fragment u))
           "{ { <http://f.com/a> <http://f.com/b> <http://f.com/c>} UNION { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} }"))))


(deftype Optional [required optional])
(derive Optional ::graph)
(defn optional [group & groups ]
  (reduce (fn [r o] (->Optional r o)) group groups))
(defmethod to-string-fragment Optional [g]
  (str "{ "
       (to-string-fragment (.required g))
       " OPTIONAL "
       (to-string-fragment (.optional g))
       " }"))

(deftest ^{:private true} gimme-options
  (let [[a b c] (map #(u (str "http://f.com/" %)) ["a" "b" "c"])]
    (let [u (optional (group [a b c])
                      (group [(? :s) :rdf/a "foo"]))]
      (is (= (collapse-whitespace (to-string-fragment u))
             "{ { <http://f.com/a> <http://f.com/b> <http://f.com/c>} OPTIONAL { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} }")))
    (let [u (optional (group [a b c])
                      (group [b :foaf/bae (? :s)])
                      (group [(? :s) :rdf/a "foo"]))]
      (is (= (collapse-whitespace (to-string-fragment u))
             "{ { { <http://f.com/a> <http://f.com/b> <http://f.com/c>} OPTIONAL { <http://f.com/b> <http://xmlns.com/foaf/0.1/bae> ?s} } OPTIONAL { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} }"
             )))))




;; "The BIND form allows a value to be assigned to a variable from a
;; basic graph pattern or property path expression. Use of BIND ends
;; the preceding basic graph pattern."

;; To make the scope a bit clearer and a bit less dependent on textual
;; proximity, we say that the BGP relevant to our Binding construct
;; is a property of the construct not just whatever happens to have
;; gone before.

(deftype Binding [variable value group])
(derive Binding ::graph)
(defn bind [[variable value] group]
  (->Binding variable (->Expr value) group))

(defmethod to-string-fragment Binding [b]
  (str "{ " (to-string-fragment (.group b))
       " BIND("
       (to-string-fragment (.value b))
       " AS " (rdf/serialize-term (.variable b))
       ")\n}\n"))

(deftest ^{:private true} bind-us-together
  (let [b (bind [(? :foo) '(concat (* (random) 2) ?s)]
                (group [(? :s) :rdf/a "foo"]))]
    (is (= (to-string-fragment b)
           "{ {\n?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"}\n BIND(concat((random() * 2), ?s) AS ?foo)\n}\n")))
  (let [b (bind [(? :foo ) "19281"]
                (group [(? :s) :rdf/a "foo"]))]
    (is (= (collapse-whitespace (to-string-fragment b))
           ;; an answer without the inner { } would be just as acceptable
           ;; here, fwiw
           "{ { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#a> \"foo\"} BIND(\"19281\" AS ?foo) }"))))


(deftype Values [variables rows])
;; deriving Values from graph is semantically dubious, but as the SPARQL grammar
;; provides it as a kind of GroupGraphPattern, entirely precedented.
(derive Values ::graph)
(defn values [variables rows]
  (->Values (map rdf/serialize-term variables)
            (map (fn [r] (map identity r))
                 rows)))

(defmethod to-string-fragment Values [v]
  (str "VALUES (" (str/join " " (map to-string-fragment (.variables v))) ") {\n"
       (str/join
        "\n"
        (map (fn [row]
               (str "( " (str/join " " (map rdf/serialize-term row)) " )"))
             (.rows v)))
       "}"))

(deftest ^{:private true} the-value-of-everything
  (let [b
        (values [(? :foo) (? :bar)]
                [["black"  "white"]
                 ["bark" "bite"]
                 ["shark" "hey man jaws was never my scene"]])]
    (is (= (collapse-whitespace (to-string-fragment b))
           (collapse-whitespace
            "VALUES (?foo ?bar) {
 ( \"black\" \"white\" )
 ( \"bark\" \"bite\" )
 ( \"shark\" \"hey man jaws was never my scene\" )}"))))
  (let [b
        (values [(? :foo) (? :bar)]
                [["black"  (u "http://www.example.com/")]])]
    (is (= (collapse-whitespace (to-string-fragment b))
           (collapse-whitespace
            "VALUES (?foo ?bar) {
 ( \"black\" <http://www.example.com/> )}")))))


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
  (let [g (group [(? :n) :foaf/copyOf (? :ed)]
                 [(? :n) :rdfs/label (? :title)])]
    (is (equal-but-for-whitespace
         (to-string-fragment (solve g))
         "WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))
    (is (equal-but-for-whitespace
         (to-string-fragment
          (solve g [(u "http://example.com/bnbgraph")]))
         "FROM <http://example.com/bnbgraph>\n WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"
         ))))

;; Operations on solution sequences to filter/rearrange/transform them

(deftype Projection [variables soln-seq])
(derive ::modified-soln-seq ::soln-seq)
(derive Projection ::projection)
(derive ::projection ::soln-seq)

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
                  (group [(? :n) :foaf/copyOf (? :ed)]
                         [(? :n) :rdfs/label (? :title)]))))
       "SELECT ?n ?ed ?title WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))
    (is (equal-but-for-whitespace
       (to-string-fragment
        (project (map ? [:n :ed :title])
                 ;; implicit solve
                 (group [(? :n) :foaf/copyOf (? :ed)]
                        [(? :n) :rdfs/label (? :title)])))
       "SELECT ?n ?ed ?title WHERE {\n?n <http://xmlns.com/foaf/0.1/copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n")))

(deftype Aggregate [grouping-variables aggregating-variables solution])
(derive Aggregate ::projection)
(defn grouping [grouping-variables aggregating-variables pattern]
  (->Aggregate grouping-variables
               (map (fn [[n v]] [n (->Expr v)])
                    (partition 2 2 aggregating-variables))
               (rdf-to-soln-seq pattern)))

(defmethod to-string-fragment Aggregate [v]
  (let [aggregates (map (fn [[var term]]
                          (str
                           "("
                           (to-string-fragment term)
                           " AS "
                           (rdf/serialize-term var)
                           ")"))
                        (.aggregating-variables v))]
    (str "SELECT "
         (str/join " \n" (map rdf/serialize-term (.grouping-variables v)))
         " \n"
         (str/join
          " \n"
          aggregates)
         (to-string-fragment (.solution v))
         " GROUP BY "
         (to-string-fragment (.grouping-variables v)))))

(deftest ^{:private true} group-by-test
  (binding [prefixes (assoc prefixes "b" "http://f.com/ns#")]
    (let [subject
          (to-string-fragment
           (grouping [(? :lastname) (? :postcode)]
                     [(? :bikes) '(count ?framenumber)
                      (? :weight) '(sum ?weight)
                      (? :value) '(sum ?cost)]
                     (group [(? :person) :b/named (? :lastname)]
                            [(? :person) :b/livesAt (? :address)]
                            [(? :address) :b/hasPostcode (? :postcode)]
                            [(? :person) :b/owns (? :bike)]
                            [(? :bike) :b/hasFrameNumber (? :framenumber)]
                            [(? :bike) :b/weighs (? :weight)]
                            [(? :bike) :b/costs (? :cost)])))]
      (is (equal-but-for-whitespace
           subject
           (str
            "SELECT ?lastname ?postcode "
            "(count(?framenumber) AS ?bikes) \n"
            "(sum(?weight) AS ?weight) \n"
            "(sum(?cost) AS ?value) "
            "WHERE {\n?person <http://f.com/ns#named> ?lastname .\n"
            " ?person <http://f.com/ns#livesAt> ?address .\n"
            "?address <http://f.com/ns#hasPostcode> ?postcode .\n"
            "?person <http://f.com/ns#owns> ?bike .\n"
            "?bike <http://f.com/ns#hasFrameNumber> ?framenumber .\n"
            "?bike <http://f.com/ns#weighs> ?weight .\n"
            "?bike <http://f.com/ns#costs> ?cost}\n"
            " GROUP BY ?lastname ?postcode"))))))


(deftype Ordering [soln-seq sort-keys])
(derive Ordering ::modified-soln-seq)

(defn order-by [soln-seq & sort-keys]
  (->Ordering (rdf-to-soln-seq soln-seq) (map ->Expr sort-keys)))

(defmethod to-string-fragment Ordering [v]
  (str (to-string-fragment (.soln-seq v))
       " ORDER BY " (str/join " " (map to-string-fragment (.sort-keys v)))))

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
                 (solve (group [(? :a) :rdfs/label (? :b)])))
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
      (or (isa? c ::projection)
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
  (binding [prefixes (assoc prefixes "shlv" "http://example.com/ns#")]
    (is (equal-but-for-whitespace
         (to-string-fragment
          (construct (group [(? :n) :shlv/isA :shlv/book]
                            [(? :n) :shlv/edition (? :ed)]
                            [(? :n) :shlv/title (? :title)])
                     (solve
                      (group [(? :n) :shlv/copyOf (? :ed)]
                             [(? :n) :rdfs/label (? :title)]))))
         "CONSTRUCT {\n?n <http://example.com/ns#isA> <http://example.com/ns#book> .\n?n <http://example.com/ns#edition> ?ed .\n?n <http://example.com/ns#title> ?title}\n WHERE {\n?n <http://example.com/ns#copyOf> ?ed .\n?n <http://www.w3.org/2000/01/rdf-schema#label> ?title}\n"))))

(deftype InsertData [graph])
(derive InsertData ::update-form)

(defmethod to-string-fragment InsertData [v]
  (str "INSERT DATA "
       (to-string-fragment (.graph v))))

(deftype DeleteData [graph])
(derive DeleteData ::update-form)

(defmethod to-string-fragment DeleteData [v]
  (str "DELETE DATA "
       (to-string-fragment (.graph v))))

(deftype Replacement [deletions insertions solution-seq])
(derive Replacement ::update-form)

(defmethod to-string-fragment Replacement [v]
  (let [insert-clause (if-let [i (.insertions v)]
                        (str "INSERT " (to-string-fragment i))
                        "")
        delete-clause (if-let [i (.deletions v)]
                        (str "DELETE " (to-string-fragment i))
                        "")]
    (str delete-clause "\n"
         insert-clause "\n"
         (to-string-fragment (.solution-seq v)))))

(defn insert
  ([graph soln-seq] (->Replacement nil graph (rdf-to-soln-seq soln-seq)))
  ([graph] (->InsertData graph)))

(defn delete
  ([graph soln-seq] (->Replacement graph nil (rdf-to-soln-seq soln-seq)))
  ([graph] (->InsertData graph)))

(defn substitute
  [deletions insertions soln-seq]
  (->Replacement deletions insertions (rdf-to-soln-seq soln-seq)))


(defn- declare-prefixes [query] query)

(defn ->string [sparql]
  (-> sparql
      rdf-to-top-level-form
      to-string-fragment
      ))

(deftest ^{:private true} select-test
  (let [s (->string
           (group [(? :a)
                   :rdfs/label
                   "Bertrand Russell Peace Foundation"]
                  [(? :a) :rdf/type :dct/Agent]
                  ))]
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
                     :rdfs/label
                     "Bertrand Russell Peace Foundation"]
                    [(? :a) :rdf/type :dct/Agent]
                    ))))]
    (is (equal-but-for-whitespace
         (delete-prefixes s)
         "SELECT ?a WHERE
{
 ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
 ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}\n"))))


(deftest ^{:private true} implicit-select-test
  (let [s (->string
           (group [(? :a)
                   :rdfs/label
                   "Bertrand Russell Peace Foundation"]
                  [(? :a) :rdf/type :dct/Agent]
                  ))]
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
      (group [(URI. "http://example.com") :foaf/nick "granddad"])
      (solve (group [(? :a) :rdfs/label (? :b)]))))
    "INSERT { \n<http://example.com> <http://xmlns.com/foaf/0.1/nick> \"granddad\"} WHERE  { ?a <http://www.w3.org/2000/01/rdf-schema#label> ?b} "))
  (is
   (equal-form
    (->string
     (insert
      (group [(URI. "http://example.com") :foaf/nick "granddad"])))
    "INSERT DATA { \n<http://example.com> <http://xmlns.com/foaf/0.1/nick> \"granddad\"}"))
  (is
   (equal-form
    (->string
     (substitute
      (group [(? :person) :rdfs/label "Old boss"])
      (group [(? :person) :rdfs/label "New boss"])
      (solve (group [(? :person) :rdfs/label "New boss"]))))
    "DELETE {\n?person <http://www.w3.org/2000/01/rdf-schema#label> \"Old boss\"}\n\nINSERT {\n?person <http://www.w3.org/2000/01/rdf-schema#label> \"New boss\"}\n\nWHERE {\n?person <http://www.w3.org/2000/01/rdf-schema#label> \"New boss\"}\n")))



(deftest ^{:private true} test-construct-query
  (let [sparql (construct
                (group [(? :n) :rdfs/label "hey"]
                       [(? :n) :foaf/familyName (? :name)])
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
