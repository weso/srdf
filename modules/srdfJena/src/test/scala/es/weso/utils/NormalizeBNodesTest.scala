package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.triples.RDFTriple
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import es.weso.utils.NormalizeBNodes._
import es.weso.utils.IOUtils._
import cats.effect.IO
import es.weso.rdf.RDFReader
import cats.implicits._

class NormalizeBNodesTest extends AnyFunSpec with Matchers {
  describe(s"Parse RDF with blank nodes") {

    it(s"Should show RDF") {
      val str =
        """|prefix : <http://e.com/>
           |:x :p _:b1 .
           |_:b1 :q :x .
           |_:b1 :r :y .
           |_:b1 :t _:b2 .
           |_:b2 :u _:b1 .
        """.stripMargin

      def iri(x: String) = IRI(s"http://e.com/" + x)

      val r = (
        RDFAsJenaModel.empty,
        RDFAsJenaModel.fromChars(str,"TURTLE",None),
        RDFAsJenaModel.fromChars(str,"TURTLE", None)
        ).tupled.use{ case (empty,rdf1,rdf2) => for {
        n1 <- normalizeBNodes(rdf1, empty)
        n2 <- normalizeBNodes(rdf2,empty)
        ss1 <- stream2io(n1.triplesWithSubject(BNode("0")))
        ss2 <- stream2io(n2.triplesWithSubject(BNode("0")))
       } yield (ss1,ss2)
      }
      r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), pair => {
        val (ss1,ss2) = pair
        val expected = List(
          RDFTriple(BNode("0"), iri("r"), iri("y")),
          RDFTriple(BNode("0"), iri("q"), iri("x")),
          RDFTriple(BNode("0"), iri("t"), BNode("1"))
      )
      ss1 should contain theSameElementsAs expected
      ss2 should contain theSameElementsAs expected
      }
      )
    }
  }

  shouldNormalizeBNodes(
    """|prefix : <http://example.org/>
       |:x :p _:1, _:2 ;
       |   :q 1 .
       |_:1 :p :y, :z .
       |:r :q :x .
      """.stripMargin,
    s"""|prefix : <http://example.org/>
        |:x :p _:B0,_:B1 ;
        |   :q 1 .
        |_:B0 :p :y, :z .
        |:r :q :x .
        |""".stripMargin,
    withLog = true
  )

  def shouldNormalizeBNodes(rdfStr: String,
                            expected: String,
                            withLog: Boolean = false
                           ): Unit = {

    def showLog(rdf: RDFReader, tag: String, withLog: Boolean): IO[Unit] = if (withLog)
    for {
      str <- rdf.serialize("N-TRIPLES")
      _ <- IO { pprint.log(str,tag) }
    } yield ()
    else IO(())

    it (s"should normalize BNodes of\n$rdfStr\nand obtain\n$expected\n") {
      val cmp = (
        RDFAsJenaModel.fromString(rdfStr, "TURTLE"),
        RDFAsJenaModel.empty,
        RDFAsJenaModel.fromString(expected, "TURTLE")
        ).tupled.use { case (rdf, builder, rdfExpected) => for {
        _ <- showLog(rdf, "rdf", withLog)
        normalized <- NormalizeBNodes.normalizeBNodes(rdf, builder)
        _ <- showLog(normalized, "normalized", withLog)
        _ <- showLog(rdfExpected, "rdfExpected", withLog)
        normalizedNodes <- normalized.subjects().compile.toList
        expectedNodes <- rdfExpected.subjects().compile.toList
       } yield (normalizedNodes, expectedNodes)
      }
      cmp.attempt.unsafeRunSync.fold(
        err => fail(s"Error: $err"),
        result => {
          val (ns,es) = result
          pprint.log(ns)
          pprint.log(es)
          ns should contain theSameElementsAs(es)
        }
      )
    }
  }

}