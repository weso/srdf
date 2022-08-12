package es.weso.rdf

import es.weso.rdf.nodes._
import munit._

class PrefixMapTest extends FunSuite {

  {
    val pm = PrefixMap
      .empty
      .addPrefix("e", IRI("http://example.org/"))
      .addPrefix("ep", IRI("http://example.org/p/"))

    qualifyTest(IRI("http://example.org/x"), pm, "e:x")
    qualifyTest(IRI("http://other.org/x"), pm, "<http://other.org/x>")
    qualifyTest(IRI("http://example.org/p/x"), pm, "ep:x")
  }

  def qualifyTest(iri: IRI, pm: PrefixMap, expected: String): Unit = {
    test(s"should qualify $iri to obtain $expected") {
      assertEquals(pm.qualify(iri), expected)
    }
  }

  {

    testGetPrefixLocalName(
      IRI("http://example.org/foo"),
      PrefixMap(
        Map(
          Prefix("a") -> IRI("http://example.org/"),
          Prefix("r") -> IRI(s"http://example.org/reference/")
        )),
      Prefix("a"),
      "foo"
    )

    testGetPrefixLocalName(
      IRI("http://example.org/reference/foo"),
      PrefixMap(
        Map(
          Prefix("a") -> IRI("http://example.org/"),
          Prefix("r") -> IRI(s"http://example.org/reference/")
        )),
      Prefix("r"),
      "foo"
    )

    def testGetPrefixLocalName(
        iri: IRI,
        pm: PrefixMap,
        expectedPrefix: Prefix,
        expectedLocalName: String): Unit = {
      test(
        s"Should getPrefixLocalName($iri) and obtain ($expectedPrefix, $expectedLocalName)") {
        pm.getPrefixLocalName(iri)
          .fold(
            e => fail(s"Error $e"),
            values => {
              val (prefix, iri, str) = values
              assertEquals(prefix, expectedPrefix)
              assertEquals(str, expectedLocalName)
            })
      }
    }
  }
}
