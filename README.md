# SRDF - Simple RDF interface

The library handles RDF using a 
[simple RDF interface](https://github.com/weso/srdf/tree/master/modules/srdf) which has 2 implementations,
one using [Apache Jena](https://jena.apache.org/)
and another one using [RDF4j](http://rdf4j.org/).

It is possible to use this library to validate RDF models from any of those RDF libraries, as well as from external SPARQL endpoints.

This interface is being used by SHACLEX.

[![Build Status](https://travis-ci.org/weso/srdf.svg?branch=master)](https://travis-ci.org/weso/srdf)
[![codecov](https://codecov.io/gh/weso/srdf/branch/master/graph/badge.svg)](https://codecov.io/gh/weso/srdf)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/67e1af0627934936b1b58796069d2a55)](https://www.codacy.com/gh/weso/srdf?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=weso/srdf&amp;utm_campaign=Badge_Grade)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/es.weso/srdf_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/es.weso/srdf_2.13)

## Installation and compilation

The project uses [sbt](http://www.scala-sbt.org/) for compilation as well as Java 1.8.

* `sbt test` compiles and runs the tests

## Publishing to OSS-Sonatype

This project uses [the sbt ci release](https://github.com/olafurpg/sbt-ci-release) plugin for publishing to [OSS Sonatype](https://oss.sonatype.org/).

##### SNAPSHOT Releases
Open a PR and merge it to watch the CI release a -SNAPSHOT version

##### Full Library Releases
1. Push a tag and watch the CI do a regular release
2. `git tag -a v0.1.0 -m "v0.1.0"`
3. `git push origin v0.1.0`
_Note that the tag version MUST start with v._


## Author & contributors

* Author: [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)

## Contribution

Contributions are greatly appreciated.
Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/weso/srdf/issues)
