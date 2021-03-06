(ns speckled.rdf
  (:import [org.apache.commons.codec.net URLCodec]
           [java.text SimpleDateFormat]
           [java.net URL URI])
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.test :as test :refer [is deftest]]))

;; reading and printing rdf terms
;; n-triples parser

(defn url-encode [term]
  (let [u (URLCodec.)] (.encode u term)))

(def ^:dynamic prefixes
  {
   "bibo" "http://purl.org/ontology/bibo/"
   "bio" "http://purl.org/vocab/bio/0.1/"
   "dct" "http://purl.org/dc/terms/"
   "event" "http://purl.org/NET/c4dm/event.owl#"
   "foaf" "http://xmlns.com/foaf/0.1/"
   "geo" "http://www.w3.org/2003/01/geo/wgs84_pos#"
   "org" "http://www.w3.org/ns/org#"
   "owl" "http://www.w3.org/2002/07/owl#"
   "rdau" "http://rdaregistry.info/Elements/u/"
   "madsrdf" "http://www.loc.gov/mads/rdf/v1#"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
   "skos" "http://www.w3.org/2004/02/skos/core#"
   "xsd" "http://www.w3.org/2001/XMLSchema#"
   "void" "http://rdfs.org/ns/void#"
   })

(defn best-prefix [string]
  (let [matches (filter (fn [[k v]] (.startsWith string v))  prefixes)]
    (first (sort-by #(- (.length (second %))) matches))))

(defn prefixize [string]
  (if-let [[ns prefix] (best-prefix string)]
    (keyword
     (if (empty? ns)
       (.substring string (.length prefix))
       (str ns ":" (.substring string (.length prefix)))))
    nil))

(deftest prefixize-test
  (binding [prefixes (assoc prefixes
                            "shlv" "http://booksh.lv/ns#"
                            "" "http://localhost:3030/"
                            )]
    (is (= (prefixize "http://booksh.lv/ns#shelfName") :shlv:shelfName)
        "replaces with prefix when found")
    (is (nil? (prefixize "https://booksh.lv/ns#shelfName"))
        "returns nil when not found")
    (is (= (prefixize "http://localhost:3030/hello") :hello))
    (binding [prefixes (assoc prefixes "shlv1" "http://booksh.lv/ns#shelf")]
      (is (= (prefixize "http://booksh.lv/ns#shelfName")
             :shlv1:Name)
          "picks longest match"))))

(def ^:dynamic rdf-base-uri "http://booksh.lv/res/")

;; these are really supposed to be IRI not just URI but TBH I have
;; NFA WTF the difference is. BRB

(defmulti u class)
(defmethod u URI [url-thing] url-thing)
(defmethod u URL [url-thing] (.toURI url-thing))
(defmethod u clojure.lang.Keyword [url-thing]
  (let [[ns_ suffix_] (str/split (name url-thing) #":" 2)
        ns (if suffix_ ns_ "")
        suffix (or suffix_ ns_)]
    (when-let [root (get prefixes ns)]
      (URI. (str root suffix)))))
(defmethod u String [url-thing]
  (.resolve (URI. rdf-base-uri) url-thing))

(deftest uri-parsing
  (binding [rdf-base-uri "http://localhost.booksh.lv/res/"
            prefixes (assoc prefixes "" "http://localhost:3030/")]
    (is (= (u :rdfs:label) (URI. "http://www.w3.org/2000/01/rdf-schema#label")))
    (is (= (u "shelf/42") (URI. "http://localhost.booksh.lv/res/shelf/42")))
    (is (= (u "http://example.com/") (URI. "http://example.com/")))
    (is (= (u :foo) (URI. "http://localhost:3030/foo")))
    ))

(defmulti serialize-term class)

(defmethod serialize-term Number [n] (pr-str n))

(defmethod serialize-term String [s]
  (str "\""
       (str/escape s {\" "\\\""
                      \\ "\\\\"
                      (char 10) "\\n"
                      (char 13) "\\r"})
       "\""))

(defmethod serialize-term java.lang.Boolean [b]
  (if b "true" "false"))

(defmethod serialize-term clojure.lang.Keyword [k]
  (if-not (u k) (throw (ex-info "unrecognised prefix in keyword "
                                {:keyword k})))
  (serialize-term (u k)))

(defn to-iso8601 [datetime]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")
              (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
    (.format fmt datetime)))

(defn from-iso8601 [datetime-string]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")
              (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))
        pos (java.text.ParsePosition. 0)]
    (.parse fmt datetime-string pos)))

(deftest read-date-test
  (is (= (from-iso8601 "2015-10-01T08:39:40Z")
         (java.util.Date. 1443688780000))))

(defmethod serialize-term java.util.Date [d]
  (str \" (to-iso8601 d) "\"^^xsd:dateTime"))

(defmethod serialize-term URI [u] (str "<" (.toString u) ">"))
(defmethod serialize-term URL [u] (str "<" (.toString u) ">"))

(deftest literally-rdf
  (is (= (serialize-term (URI. "http://example.com"))
         "<http://example.com>"))
  (is (= (serialize-term "donkey") "\"donkey\""))
  (is (= (serialize-term "James \"The Turkey\" Mason")
         "\"James \\\"The Turkey\\\" Mason\""))
  (is (= (serialize-term "James\nMason")
         "\"James\\nMason\""))
  (is (= (serialize-term (java.util.Date. 1443688780575))
         "\"2015-10-01T08:39:40Z\"^^xsd:dateTime")))


(def n-triple-parser (insta/parser (io/resource "n-triples.bnf")))

(defmulti make-literal (fn [string iriref] iriref))

(defmethod make-literal (u :xsd:boolean) [string _]
  (case string
    "true" true
    "false" false))

(defmethod make-literal (u :xsd:string) [string _]
  string)

(defmethod make-literal (u :xsd:dateTime) [string _]
  (from-iso8601 string))

(defn visit-node [branch]
  (if (vector? branch)
    (case (first branch)
      :IRIREF
      (let [[_< [_iri_tok & letters] _>] (rest branch)
            iri (str/join letters)]
        (or (prefixize iri)
            (URI. iri)))
      :STRING_LITERAL (str/join (rest branch))
      :STRING_LITERAL_QUOTED (let [[_ string _] (rest branch)] string)
      :literal
      (let [[s & [lang-or-caret iri]] (rest branch)
            lang (and (= (first lang-or-caret) :LANGTAG)
                      (str/join (rest (rest lang-or-caret))))
            iri (and (= lang-or-caret "^^") iri)]
        (make-literal s (u (or iri  :xsd:string))))
      :WS ""
      :UCHAR (let [[_ & hexs] (rest branch)]
               (String.
                (Character/toChars
                 (Integer/parseInt (str/join (map second hexs)) 16))))
      :triple (let [m (reduce (fn [m [k v]] (assoc m k v)) {}
                              (rest branch))]
                [:triple [(:subject m) (:predicate m) (:object m)]])
      branch)
    branch))

(defn parse-n-triples [in]
  (->> in
       (insta/parse n-triple-parser)
       (walk/postwalk visit-node)
       (tree-seq #(and (vector? %)
                       (keyword? (first %))
                       (not (= (first %) :triple)))
                 #(rest %))
       (filter #(= (first %) :triple))
       (map second)))

(deftest n-triples-test
  (let [ls (str/split (slurp (io/resource "n-triples-test.nt")) #"\n")]
    (doall
     (map #(is  (count (parse-n-triples %))) ls))))

(deftest convert-literals-test
  (let [c #(nth (first (parse-n-triples %)) 2)]
    (is (= (c "<http://example/s> <http://example/p> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n") true))
    (is (= (c "<http://example/s> <http://example/p> \"false\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n") false))
    (is (= (c "<http://example/s> <http://example/p> \"fish\"^^<http://www.w3.org/2001/XMLSchema#string> .\n") "fish"))
    (is (= (c "<http://example/s> <http://example/p> \"chips\" .\n") "chips"))
    (is (= (c "<http://example/s> <http://example/p> \"2015-10-01T08:42:40Z\"^^<http://www.w3.org/2001/XMLSchema#dateTime> .\n")
           #inst "2015-10-01T08:42:40.000-00:00"))))
