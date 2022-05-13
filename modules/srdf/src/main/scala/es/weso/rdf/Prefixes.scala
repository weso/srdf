package es.weso.rdf

import es.weso.rdf.nodes.IRI

/**
 * Common Prefixes for RDF
 */
object PREFIXES {

  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val owl = IRI("http://www.w3.org/2002/07/owl#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val sh = IRI("http://www.w3.org/ns/shacl#")

  lazy val `xsd:string`: IRI = xsd + "string"
  lazy val `xsd:byte`: IRI = xsd + "byte"
  lazy val `xsd:dateTime`: IRI = xsd + "dateTime"
  lazy val `xsd:decimal`: IRI = xsd + "decimal"
  lazy val `xsd:double`: IRI = xsd + "double"
  lazy val `xsd:int`: IRI = xsd + "int"
  lazy val `xsd:integer`: IRI = xsd + "integer"
  lazy val `xsd:long`: IRI = xsd + "long"
  lazy val `xsd:positiveInteger`: IRI = xsd + "positiveInteger"
  lazy val `xsd:negativeInteger`: IRI = xsd + "negativeInteger"
  lazy val `xsd:nonNegativeInteger`: IRI = xsd + "nonNegativeInteger"
  lazy val `xsd:nonPositiveInteger`: IRI = xsd + "nonPositiveInteger"
  lazy val `xsd:short`: IRI = xsd + "short"
  lazy val `xsd:unsignedLong`: IRI = xsd + "unsignedLong"
  lazy val `xsd:unsignedInt`: IRI = xsd + "unsignedInt"
  lazy val `xsd:unsignedShort`: IRI = xsd + "unsignedShort"
  lazy val `xsd:unsignedByte`: IRI = xsd + "unsignedByte"
  lazy val `xsd:float`: IRI = xsd + "float"
  lazy val `xsd:anyUri`: IRI = xsd + "anyUri"
  lazy val `xsd:boolean`: IRI = xsd + "boolean"

  lazy val `rdf:type`: IRI = rdf + "type"
  lazy val `rdf:nil`: IRI = rdf + "nil"
  lazy val `rdf:first`: IRI = rdf + "first"
  lazy val `rdf:rest`: IRI = rdf + "rest"
  lazy val `rdf:langString`: IRI = rdf + "langString"

  lazy val `rdfs:label`: IRI = rdfs + "label"
  lazy val `rdfs:subClassOf`: IRI = rdfs + "subClassOf"

  lazy val `sh:alternativePath`: IRI = sh + "alternativePath"
  lazy val `sh:inversePath`: IRI = sh + "inversePath"
  lazy val `sh:oneOrMorePath`: IRI = sh + "oneOrMorePath"
  lazy val `sh:zeroOrMorePath`: IRI = sh + "zeroOrMorePath"
  lazy val `sh:zeroOrOnePath`: IRI = sh + "zeroOrOnePath"

  val basicMap: Map[String, IRI] =
    Map(
      "rdf" -> rdf,
      "xsd" -> xsd,
      "rdfs" -> rdfs,
      "owl" -> owl,
      "sh" -> sh
    )

}
