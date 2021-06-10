package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.NormalizeBNodes._
import es.weso.utils.IOUtils._
import cats.effect._
import es.weso.rdf.RDFReader
import cats.implicits._
import munit._
import es.weso.utils.internal.CollectionCompat.LazyList

class NormalizeBNodesTest extends CatsEffectSuite {
  
  test(s"Should show RDF") {
      val str =
        """|prefix : <http://e.com/>
           |:x :p _:b1 .
           |_:b1 :q :x .
           |_:b1 :r :y .
           |_:b1 :t _:b2 .
           |_:b2 :u _:b1 .
        """.stripMargin

      def iri(x: String) = IRI(s"http://e.com/" + x)


      val r = for {
        res1 <- RDFAsJenaModel.empty
        res2 <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        res3 <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        v <- (res1,res2,res3).tupled.use { case (empty, rdf1, rdf2) => for {
         n1 <- normalizeBNodes(rdf1, empty)
         n2 <- normalizeBNodes(rdf2, empty)
         ss <- stream2io(n1.triplesWithSubject(BNode("0")))
         } yield ss } 
       } yield v
      r.map(_.sorted).assertEquals(LazyList(
          RDFTriple(BNode("0"), iri("r"), iri("y")),
          RDFTriple(BNode("0"), iri("q"), iri("x")),
          RDFTriple(BNode("0"), iri("t"), BNode("1"))
        ).sorted)
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
        _ <- IO {
          pprint.log(str, tag)
        }
      } yield ()
    else IO(())

    test(s"should normalize BNodes of\n$rdfStr\nand obtain\n$expected\n") {
      val cmp = for {
        res1 <- RDFAsJenaModel.fromString(rdfStr, "TURTLE")
        res2 <- RDFAsJenaModel.empty
        res3 <- RDFAsJenaModel.fromString(expected, "TURTLE")
        v <- (res1,res2,res3).tupled.use { case (rdf, builder, rdfExpected) => for {
        _ <- showLog(rdf, "rdf", withLog)
        normalized <- NormalizeBNodes.normalizeBNodes(rdf, builder)
        _ <- showLog(normalized, "normalized", withLog)
        _ <- showLog(rdfExpected, "rdfExpected", withLog)
        normalizedNodes <- normalized.subjects().compile.toList
        expectedNodes <- rdfExpected.subjects().compile.toList
       } yield (normalizedNodes, expectedNodes) }
      } yield v
      cmp.map(pair => assertEquals(pair._1.sorted,pair._2.sorted))
  }
 }

}