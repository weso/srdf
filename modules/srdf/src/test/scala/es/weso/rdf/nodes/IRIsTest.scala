package es.weso.rdf.nodes

import org.scalatest._
import funspec.AnyFunSpec
import matchers.should.Matchers


class IRIsTest extends AnyFunSpec with Matchers with TryValues {

  describe("IRIsTest") {
    it("can add string to an IRI") {
      val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
      val xsd_string = xsd + "string"
      xsd_string.str should be("http://www.w3.org/2001/XMLSchema#string")
    }
  }
}