package es.weso.rdf

import munit._
import es.weso.rdf.nodes._

class PrefixesTest extends FunSuite {

    test("can get xsd prefix") {
      val xsd = PREFIXES.xsd
      val iri_xsd = IRI("http://www.w3.org/2001/XMLSchema#")
      assertEquals(iri_xsd.str, xsd.str)
    }
}