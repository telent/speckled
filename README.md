# Speckled - a Sparql Dsl

* SPARQL DSL
* network code for talking to SPARQL endpoint

Originally extracted from booksh.lv

## Usage

### Creating Sparql 

    (require '[speckled.sparql :refer [->string group ?]])
    (->string
        (solve
         (group [(? :a)
                 :rdfs:label
                 "Bertrand Russell Peace Foundation"]
                [(? :a) :rdf:type :dct:Agent] )
 

### Performing a query against the server

> (sq/query (comp http/post :body) "http://localhost:3030/ds/"
            "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")

The first argument is the function that your preferred HTTP client
uses to do a POST request.  It should accept the same arguments as
`clj-http.client/post` _and return the response body as a string_.  For
example, to use the client from HTTP-Kit you might say

> (sq/query (comp deref org.httpkit.client/post)
            "http://localhost:3030/ds/"
            "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")

or to use Aleph (which as of 0.4.1 doesn't support `{:as :text}`)

> (sq/query (comp slurp :body deref http/post)
            "http://localhost:3030/ds/"
            "SELECT  ?p ?o WHERE { </foo/2> ?p ?o }")




## License

Copyright © 2015,2016 Daniel Barlow

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
