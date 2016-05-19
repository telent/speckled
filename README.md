# Speckled - a library for talking to SPARQL endpoints

* SPARQL DSL
* network code for talking to SPARQL endpoint

Originally extracted from booksh.lv

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


### terms: literals, IRIs, variables

- Literals are represented as strings (no support for language selection yet)

- URL and URI objects are converted to IRI refs.  You can say `(u "foo")` to easily create a URL relative to `speckled.rdf/rdf-base-uri`

- keywords are also converted to IRI, if they match one of the declared prefixes in `speckled.rdf/prefixes`

- Variables are represented as `Variable` objects.  You can create them with the `?` function 

### triples

A triple is a vector of three terms: [subject relation object]

### graphs: groups of triples (and groups of groups)

### solution, solution sequence

Given a graph with some variables, a solution is some set of values
for each of the variables which cause make the graph match data in the data
store

A solution sequence is a collection of all the possible solutions for some graph

### solution sequence modifiers

Filter, narrow or re-order the solution sequence using operators like `project` (equates to SPARQL `SELECT`), `distinct`, `limit` etc

### Top level forms

These say what we want done with the solution sequence.  In SPARQL these are

  * SELECT - tell me the values
  * CONSTRUCT - make up some new triples
  * INSERT - make up some new triples and add them to the data store
  * etc

## Convenience shortcuts

1. you can usually omit writing `solve` where its necessity can be
inferred: any operator that requires a solution sequence will wrap its
argument in one if it isn't already

2. the `SELECT` clause in a SPARQL query serves dual duty: first as a
solution sequence modifier which narrows the result sequence by
removing some of the variables from it (in relational algebra we would
describe this as a "projection" or "restriction"), and second to
indicate the form of the result to the SPARQL processor - that we want
to know the values of the variables (as opposed to, e.g. whether a
solution exists (`ASK`) or a new set of triples that use them
(`CONSTRUCT`)).  In Speckled we separate these two roles: we use
`project` for the "Projection" operator that narrows the solution
sequence, and then we use `select` to indicate the result format.

3. following on from point 2, you can omit `project` if you don't want
to restrict the variables returned, and if you omit the operator to
specify the top level form type (`select`, `construct`, `ask`,
`insert` ...) then it will assume that you wanted a `SELECT`. So you
can write simple queries much more concisely: e.g.

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
