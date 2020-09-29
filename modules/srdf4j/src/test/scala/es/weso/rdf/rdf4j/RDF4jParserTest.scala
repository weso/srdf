package es.weso.rdf.rdf4j

import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes._
import org.scalatest._
import cats.effect._

class RDF4jParserTest extends FunSpec with Matchers with EitherValues with OptionValues {

  describe("RDF4jParser") {
    it(s"Should parse simple Turtle string") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val r = RDFAsRDF4jModel.fromChars(str,"Turtle",None).use(rdf => for {
       number <- rdf.getNumberOfStatements()
      } yield number)
      r.unsafeRunSync() should be(2)
    }
  }

  describe(s"RDF4j API as SRDF") {
    it(s"Should be able to parse prefix maps") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val r = RDFAsRDF4jModel.fromChars(str,"Turtle",None).use(rdf =>
        rdf.getPrefixMap
      )
      r.unsafeRunSync.getIRI("").value should be(IRI("http://example.org/"))
    }

    it(s"Should be able to extend the prefix map") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val r = RDFAsRDF4jModel.fromChars(str,"Turtle",None).use(rdf => for {
        rdf1 <- rdf.addPrefixMap(PrefixMap(Map(Prefix("kiko") -> IRI("http://kiko.org"))))
        pm <- rdf1.getPrefixMap
      } yield pm)
      val pm = r.unsafeRunSync
      pm.getIRI("kiko").value should be(IRI("http://kiko.org"))
      pm.getIRI("pepe") should be(None)
    }

    it(s"Should be able to get subjects") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val r = RDFAsRDF4jModel.fromChars(str,"Turtle",None).use(rdf => for {
        ts <- rdf.triplesWithSubject(IRI("http://example.org/x")).compile.toList
      } yield ts)
      r.unsafeRunSync.size should be(1)
    }
  }
}
