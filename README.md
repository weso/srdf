# SRDF - Simple RDF interface

The library handles RDF using a 
[simple RDF interface](https://github.com/weso/srdf/tree/master/modules/srdf) which has 2 implementations,
one using [Apache Jena](https://jena.apache.org/)
and another one using [RDF4j](http://rdf4j.org/).

It is possible to use this library to validate RDF models from any of those RDF libraries, as well as from external SPARQL endpoints.

This interface is being used by SHACLEX.

[![Build Status](https://travis-ci.org/weso/srdf.svg?branch=master)](https://travis-ci.org/weso/srdf)

## Installation and compilation

The project uses [sbt](http://www.scala-sbt.org/) for compilation as well as Java 1.8.

* `sbt test` compiles and runs the tests

## Author & contributors

* Author: [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)

## Contribution

Contributions are greatly appreciated.
Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/weso/srdf/issues)
