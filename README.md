# Speckled - a library for talking to SPARQL endpoints

* SPARQL DSL
* network code for talking to SPARQL endpoint

Status as of June 2016: under active (if slow) development,
pre-pre-Alpha, APIs will change probably without notice.  Hopefully
there is enough here to illustrate the direction in which we think
we're going, but there are significant gaps which I will probably only
get to when I actually need the functionality.

As of June 2017: still nominally being developed, but free time is scarce.

## Installation

It's on clojars.  Depending on whichever your favourite build tool is, you
should just be able to add it to your project dependencies in
`project.clj`/`build.boot`/`pom.xml`/`Gemfile` (OK, perhaps not
that last one so much)

[![Clojars Project](https://img.shields.io/clojars/v/telent/speckled.svg)](https://clojars.org/telent/speckled) 
[![CircleCI](https://circleci.com/gh/telent/speckled.svg?style=svg)](https://circleci.com/gh/telent/speckled)

## Build from source

It's [on github](https://github.com/telent/speckled) and it requires
Leiningen.  Clone it and run `lein jar` or whatever.

To run the tests, do `lein test src/speckled/*.clj`.  I like to put
unit tests alongside the code that they're testing: it reminds me to
keep them up to date and provides (perhaps) useful documentation for
anyone reading the code.

# SPARQL DSL

Most of Speckled is a DSL for creating SPARQL queries.  Here's a
fairly simple example to show it off a bit

    (ns my-namespace
      (:require [speckled.dsl :refer :all]
                [speckled.rdf :refer [u]]))

    ;; (speckled.dsl is written with some care about preserving its
    ;; public/private interface and the intention is that it should be
    ;; safe to :refer :all without spamming your application
    ;; namespaces with incidental crud.  Please flag if/where you find
    ;; this not to be the case)

    (->string
     (select
      (project [(? :a)]
               (solve
                (group [(? :a)
                        :rdfs:label
                        "Bertrand Russell Peace Foundation"]
                       [(? :a) :rdf:type :dct:Agent] )))))
    =>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdau: <http://rdaregistry.info/Elements/u/>
    [... more prefix declarations omitted for brevity ...]
    SELECT *  WHERE {
      ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
      ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}

## Language structure

The structure of the language was designed by looking at the SPARQL
specification through half-closed eyes: it's inspired by the official
SPARQL grammar but takes some liberties in the interests of
pragmatism/conciseness/convenience.  From the inside out:


### Terms: literals, IRIs, variables

Conversions to and from literals are controlled by the multimethods

* `speckled.rdf/serialize-term` which converts a Clojure value to an
RDF literal, and

* `speckled.rdf/make-literal` which converts an RDF literal into a Clojure
value.  You can add methods for your own XSD datatype

Out of the box, Speckled knows how to convert
- Strings, Numbers and Booleans, to simple literals
- java.util.Date, to xsd:dateTime


- URL and URI objects are converted to IRI refs.  You can say `(u "foo")` to easily create a URL relative to `speckled.rdf/rdf-base-uri`

- a keyword of the form `:prefix:word` is converted into an IRI ref by splitting into `prefix` and `word` and then checking `prefix` against the declared prefixes in `speckled.rdf/prefixes`

- Variables are represented as `Variable` objects.  You can create them with the `?` function 

### Triples

A triple is a vector of three terms: [subject relation object]

### Graph patterns: pattern-matching the dataset

A
[group graph pattern](https://www.w3.org/TR/sparql11-query/#GroupPatterns) is
an array of triples containing zero or more variables.  A
_solution_ for the pattern comprises a value for each of those
variables chosen so that the graph pattern matches some subset of the
triples in the data store.  A _solution sequence_ is a collection of
all the possible solutions for the graph pattern.

### Inline literal data block

An
[inline data block](https://www.w3.org/TR/sparql11-query/#inline-data)
specifies one or more variable names and some literal values for each
of them, to specify a solution sequence which does not depend on the
data store being queried.  This solution sequence can be (is, usually)
combined with solution sequences from graph patterns.

```
 (values [(? :foo) (? :bar)]
   [["black"  "white"]
    ["bark" "bite"]
    ["shark" "hey man jaws was never my scene"]]))
```

### Combining query patterns

Graph patterns (and data blocks) may be combined and augmented in
various ways.  If you need SPARQL composition operations which are not
in this list, the most likely reason they aren't implemented is that I
haven't got to them yet.  Patches, as they say, welcome.


* `(union g1 g2 ... )` - "a means of combining
  graph patterns so that one of several alternative graph patterns may
  match. If more than one of the alternatives matches, all the
  possible pattern solutions are found."  See [SPARQL `UNION` keyword](https://www.w3.org/TR/sparql11-query/#alternatives)

* `with-graph` - require that a group pattern be matched by triples in a named
  graph instead of in the default graph. See [SPARQL `GRAPH` keyword](https://www.w3.org/TR/sparql11-query/#queryDataset)

* `(optional g0 g1 )` is a bit like a left join: it defines a graph
  pattern in which the pattern `g0` _must_ match and `g1` may or may
  not match.
  See
  [SPARQL `OPTIONAL` keyword](https://www.w3.org/TR/sparql11-query/#optionals).  
  For each possible solution to `g0`, if `g1` matches then it will add
  its bindings to the solution, and if it doesn't then it creates no
  additional bindings but does not cause the solution to be rejected.
  Note that `g1` must match in its entirety or not at all: if you have
  two or more optional patterns that may match independently of each
  other, you cannot just mush them together in `g1`.

* `(optional g0 g1 g2 gn)`

  As a syntactic convenience, multiple optional graphs can be supplied
  to the `optional` clause.  This is treated as though it were
  written 
  
  `(optional (optional (optional g0 g1) g2) gn)`

* `(filter-solns g expr )` acccepts a group pattern `g` and filters the
  solutions it produces such that the result is only the solutions for
  which the `expr` is true.  See SPARQL FILTER, but note that the
  filter expression in Speckled is *outside* the group (i.e. [where it
  logically ought to be](https://www.w3.org/TR/sparql11-query/#sparqlCollectFilters)) not inside it where SPARQL syntax expects it.

* `(bind [(? :v) expr (? :v2) expr2 ...] g1)` to define new variable
 names `v`, `v2` etc and give them values derived from the variables in `g1`.
 See [SPARQL `BIND`](https://www.w3.org/TR/sparql11-query/#bind)

* `(grouping [(? :v1) (? :v2) ...] [(? :v3) '(min x) (? :v4) '(count y) ...] g0)`

   Group the solutions of `g0` into subsequences such that in each
   subsequence, every solution has the same values of `?v1` `?v2` etc.
   Then apply the aggregating functions `(min x)`, `(count y)` etc
   across each subsequence, to create a new solution
   sequence which has variables `?v1 ?v2 ?v3 ?v4` and as many
   solutions as there were subsequences.
   
   https://www.w3.org/TR/sparql11-query/#groupby

#### Functions

The `bind` and `grouping` operations allow you to call functions which
take literals and variable references as parameters.  Write them as
quoted lists: for example, 

```
     (bind [(? :evid)
            '(iri (concat "http://example.com/events/" (struuid)))]
           (group ...))
```

This is converted to the SPARQL
`iri(concat("http://example.com/events/", struuid()))`.  A small
selection of arithmetic functions are recognised and converted to infix.


### Solution sequence modifiers

You can apply solution sequence modifiers to your query, in order to
filter, narrow or re-order the solution sequence computed by that query.

* `project` : allows you to name the specific variables you want from from each solution (instead of getting all of them)
* `distinct` : remove identical solutions from the sequence
* `order-by` : change the order that solutions are returned in: e.g. `(order-by
    soln-seq '?date)` or `(order soln-seq '(desc ?date))`
* `limit` : 'top and tail' the solution sequence

#### A short digression about `project`

The `SELECT` clause in a SPARQL query serves dual duty

* first as a solution sequence modifier which narrows the result
sequence by removing some of the variables from it (in relational
algebra we would describe this as a "projection" or "restriction"),

* and second to indicate the desired form of the result to the SPARQL
processor - specifically, that we want to know the values of the
variables (as opposed to, e.g. whether a solution exists (`ASK`) or a
new set of triples that use them (`CONSTRUCT`)).

In Speckled we separate these two roles: we use `project` for the
"Projection" operator that narrows the solution sequence, and then we
use `select` to indicate the result format.

Speckled is not dogmatic about making you write `project` and `select`
explicitly in your queries if it can figure out that it needed either
or both of them (see "Convenience shortcuts" below), but you will
probably have an easier ride if your mental model embraces this
distinction instead of treating it as magic :-)

### Top level forms

These say what we want done with the solution sequence.  

  * `select` is SPARQL SELECT - tell me the values
  * `construct` is SPARQL CONSTRUCT - make up some new triples
  * `(insert graph soln-seq)` is SPARQL INSERT - make up some new triples and add them
    to the data store.  If soln-seq is omitted we use `INSERT DATA`
  * `delete` is SPARQL DELETE (as per INSERT)
  * `(substitute deletions insertions soln-seq)` is the least worst name we can think of for SPARQL `DELETE-INSERT`


## Convenience shortcuts

1. you can omit `project` if you don't want to restrict the variables
returned, and if you omit the operator to specify the top level form
type (`select`, `construct`, `ask`, `insert` ...) then it will assume
that you wanted a `SELECT`. So you can write simple queries much more
concisely: e.g.

```
     (->string
        (group [(? :a)
                :rdfs:label
                "Bertrand Russell Peace Foundation"]
               [(? :a) :rdf:type :dct:Agent] )))))

```
to get

     SELECT *  WHERE {
       ?a <http://www.w3.org/2000/01/rdf-schema#label> "Bertrand Russell Peace Foundation" .
       ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>
     }

or something, modulo whitespace changes, very much like it.


# Talking to SPARQL servers

    (def post-http (comp http/post :body))

    (speckled.net/query post-http "http://localhost:3030/ds/"
                        "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")

Presently there are separate functions for different top level forms

* for SELECT use `query`
* for INSERT use `insert-store`
* for CONSTRUCT use `query-graph`

This is ugly and I will look at unifying these interfaces just as soon as I have a good idea how to

## Choose your own HTTP library

(If you don't have your own HTTP library you can always choose to use
somebody else's ;-)

The first argument to `query` is the function that your preferred HTTP
client uses to do a POST request.  It should accept the same arguments
as `clj-http.client/post` _and return the response body as a string.
For example, to use the client from HTTP-Kit you might say

    (def post-http (comp deref org.httpkit.client/post))

or to use Aleph (which returns a Manifold deferred instead of giving
you a response directly, and as of 0.4.1 doesn't support `{:as
:text}`) you could do 

    (def post-http (comp slurp :body deref http/post))



## License

Copyright © 2015,2016 Daniel Barlow

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
