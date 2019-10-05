# srdfJena module

Implementations of the [SRDF](https://github.com/labra/shaclex/tree/master/modules/srdf) interface based on [Apache Jena](https://jena.apache.org/)

Some classes:
- [RDFAsJenaModel](https://github.com/weso/srdf/blob/master/modules/srdfJena/src/main/scala/es/weso/rdf/jena/RDFAsJenaModel.scala) is an implementation of the SRDF interface than can be used to declare RDFReader's and RDFBuilder's on top of Jena Models.
- [Endpoint](https://github.com/weso/srdf/blob/master/modules/srdfJena/src/main/scala/es/weso/rdf/jena/Endpoint.scala) is an implementation of SRDF based on SPARQL endpoints.
- [Compound](https://github.com/weso/srdf/blob/master/modules/srdfJena/src/main/scala/es/weso/rdf/jena/Compound.scala) is an implementation of SRDF reader that is composed from one or mode Jena Models and one or more Endpoints.
- [RDFFromWeb](https://github.com/weso/srdf/blob/master/modules/srdfJena/src/main/scala/es/weso/rdf/jena/RDFFromWeb.scala) is an implementation of SRDF that is based on resource dereferencing.
