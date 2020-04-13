package es.weso.rdf.parser

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import cats.effect.IO
import cats.MonadError

class RDFParserBNodesTest extends AnyFunSpec with Matchers with RDFParser with EitherValues {

  describe("RDFParser for BNodes") {

      it("check if bnode Ids are maintained") {
        val cs = """|prefix : <http://example.org/>
                   |_:x :p _:y .
                   |_:y :q 1""".stripMargin
        val r = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          objs <- rdf.triplesWithSubject(BNode("x")).compile.toList
        } yield (objs)
        val eitherValue = MonadError[IO,Throwable].attempt(r).unsafeRunSync
        eitherValue.fold(e => fail(s"Parse error: $e"), objs => {
          objs match {
            case Nil => fail(s"No triples with subject _:x")
            case node :: Nil => node.obj should be(BNode("y"))
            case _ => fail(s"More than one triples with subject _:x: ${objs}") 
          }
        })
  }
 }
}