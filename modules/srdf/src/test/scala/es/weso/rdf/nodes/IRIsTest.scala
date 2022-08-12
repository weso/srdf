package es.weso.rdf.nodes

import munit._

class IRIsTest extends FunSuite {

  test("can add string to an IRI") {
    val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
    val xsd_string = xsd + "string"
    assertEquals(xsd_string.str, "http://www.w3.org/2001/XMLSchema#string")
  }
}
