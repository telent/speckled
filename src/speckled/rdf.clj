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

;; these are really supposed to be IRI not just URI but TBH I have
;; NFA WTF the difference is. BRB

(defmulti u class)
(defmethod u URI [url-thing] url-thing)
(defmethod u URL [url-thing] (.toURI url-thing))
(defmethod u String [url-thing] (URI. url-thing))

(defn xsd [x] (u (str "http://www.w3.org/2001/XMLSchema#" x)))

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

(defmethod make-literal (xsd "boolean") [string _]
  (case string
    "true" true
    "false" false))

(defmethod make-literal (xsd "string") [string _]
  string)

(defmethod make-literal (xsd "dateTime") [string _]
  (from-iso8601 string))

(defn visit-node [branch]
  (if (vector? branch)
    (case (first branch)
      :IRIREF
      (let [[_< [_iri_tok & letters] _>] (rest branch)
            iri (str/join letters)]
        (URI. iri))
      :STRING_LITERAL (str/join (rest branch))
      :STRING_LITERAL_QUOTED (let [[_ string _] (rest branch)] string)
      :literal
      (let [[s & [lang-or-caret iri]] (rest branch)
            lang (and (= (first lang-or-caret) :LANGTAG)
                      (str/join (rest (rest lang-or-caret))))
            iri (and (= lang-or-caret "^^") iri)]
        (make-literal s (or iri  (xsd "string"))))
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
