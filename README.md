# Speckled - a Sparql Dsl

* SPARQL DSL
* network code for talking to SPARQL endpoint

Originally extracted from booksh.lv

# Usage

## Creating Sparql 

Speckled is a DSL for creating SPARQL queries.  The strcture of the
language does not quite correspond to the sparql parse tree, but looks
reasonably similar. We have

* terms: literals, IRIs, variables
* triples: (subject relation object) 
* graphs: groups of triples (and groups of groups)
* solution: given a graph with some variables, a set of values for all the variables which make the graph match data in the data store
* solution sequence: all the possible solutions for some graph
* solution sequence modifiers: filter, narrow or re-order the solution sequence
* top level forms, which say what we want done with the solution sequence

  * SELECT - tell me the values
  * CONSTRUCT - make up some new triples
  * INSERT - make up some new triples and add them to the data store
  * etc

We represent triples as vectors of three elements and everything else
as nested structs: convenience functions are provided to write them concisely.
The primary entry point to convert a top level form into a string that can be sent to a SPARQL engine is `->string`

    (require '[speckled.sparql :refer [->string group solve ?]])
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

This is the fully general form of a query.  Some points are worthy of note

1. you can usually omit writing `solve` where its necessity can be
inferred: any operator that requires a solution sequence will wrap its
argument in one if it isn't already

2. the `SELECT` clause in a SPARQL query serves dual duty: first as a
solution equence modifier which narrows the result sequence by
removing some of the variables from it, and second to indicate the
form of the result to the SPARQL processor - that we want to know the
values of the variables (as opposed to, e.g. whether a solution exists
(`ASK`) or a new set of triples that use them (`CONSTRUCT`)).  In
Speckled we separate these two roles: we use `project` for the
"Projection" operator that narrows the solution sequence, and then we
use `select` to indicate the result format.

3. following on from point 2, you can omit `project` if you don't want
to restrict the variables returned, and if you omit the operator to
specify the top level form type (`select`, `construct`, `ask`,
`insert` ...) then it will infer that you wanted a `SELECT`. So you
could write

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

## Performing a query against the server

    (sq/query (comp http/post :body) "http://localhost:3030/ds/"
              "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")

The first argument is the function that your preferred HTTP client
uses to do a POST request.  It should accept the same arguments as
`clj-http.client/post` _and return the response body as a string_.  For
example, to use the client from HTTP-Kit you might say

    (sq/query (comp deref org.httpkit.client/post)
              "http://localhost:3030/ds/"
              "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")

or to use Aleph (which as of 0.4.1 doesn't support `{:as :text}`)

    (sq/query (comp slurp :body deref http/post)
              "http://localhost:3030/ds/"
              "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")




## License

Copyright © 2015,2016 Daniel Barlow

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
