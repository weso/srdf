package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.rdf.triples.RDFTriple
import org.scalatest._
import cats._
import cats.effect._
import es.weso.utils.IOUtils._

class RelativeURIsTest extends FunSpec with Matchers {
  describe("Relative URIs") {
    it(s"Should parse Turtle with relative URIs") {
      val str = """<x> <p> <y>"""
      val base = Some(IRI("internal://base/"))
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", base)
        x <- fromES(IRI.fromString("x", base))
        p <- fromES(IRI.fromString("p",base))
        y <- fromES(IRI.fromString("y",base))
        ts <- rdf.triplesWithSubject(x).compile.toList
        serialized <- rdf.serialize("TURTLE",base)
      } yield {
        (ts,rdf,serialized,x,p,y)
      }
      val pairs = MonadError[IO,Throwable].attempt(r).unsafeRunSync 
      
      pairs.fold(e => fail(s"Error: $e"),
        pair => {
          val (ts,rdf,str,x,p,y) = pair
          println(str)
          ts should contain theSameElementsAs(Set(RDFTriple(x,p,y)))
        }
      )
    }
  }

}