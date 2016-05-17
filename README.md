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
* groups of triples (and groups of groups)
* graph
* solution: given a graph, some set of values for all the variables in that graph which match data in the data store
* solution sequence: all the possible solutions for some graph
* solution sequence modifiers, which filter, narrow or re-order the solution sequence
* top level forms, which say what we want done with the solution sequence
** SELECT - tell me the values
** CONSTRUCT - make up some new triples
** INSERT - make up some new triples and add them to the data store
** etc

We represent triples as vectors of three elements and everything else
as nested structs for which there are convenience functions to write concisely.
The primary entry point to convert a top level form into a string that can be sent to a SPARQL engine is `->string`

    (require '[speckled.sparql :refer [->string group solve ?]])
    (->string
        (solve
         (group [(? :a)
                 :rdfs:label
                 "Bertrand Russell Peace Foundation"]
                [(? :a) :rdf:type :dct:Agent] )))
    =>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdau: <http://rdaregistry.info/Elements/u/>
    [... more prefix declarations omitted for brevity ...]
    SELECT *  WHERE {
      ?a <http://www.w3.org/2000/01/rdf-schema#label> \"Bertrand Russell Peace Foundation\" .
      ?a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/dc/terms/Agent>}
    

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
