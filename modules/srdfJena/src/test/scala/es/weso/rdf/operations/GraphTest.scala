package es.weso.rdf.operations

import cats.syntax.show._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.IOUtils._
import cats.effect.IO
import es.weso.rdf.RDFReader
// import org.scalatest.matchers.should.Matchers._
import es.weso.utils.internal.CollectionCompat._
import munit.CatsEffectSuite

class GraphTest extends CatsEffectSuite {

  def ex: IRI = IRI("http://example.org/")

  def iri(s: String): IRI = ex + s

  def bnode(s: String): BNode = BNode(s)

  def int(n: Int) : IntegerLiteral = IntegerLiteral(n, n.toString)

  def string(s: String): StringLiteral  = StringLiteral(s)

  def rdfHTML(s: String): RDFhtmlStringLiteral  = RDFhtmlStringLiteral(s)

  shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :y .
      """.stripMargin,
      LazyList(iri("x"), iri("y"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x .
      """.stripMargin,
      LazyList(iri("x"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:y :p :z, :x .
      """.stripMargin,
      LazyList(iri("x"), iri("y"), iri("z"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:y :p :z, :x .
         |:r :q :x .
      """.stripMargin,
      LazyList(iri("x"), iri("y"), iri("z"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y ;
         |   :q :t .
         |:y :p :z, :x .
         |:r :q :x .
      """.stripMargin,
      LazyList(iri("x"), iri("y"), iri("z"), iri("t"))
    )

    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p _:1, _:2 ;
         |   :q 1 .
         |_:1 :p :y, :z .
         |:r :q :x .
      """.stripMargin,
      LazyList(
        iri("x"), iri("y"), iri("z"),
        bnode("1"), bnode("2"),
        int(1)
      ),
      true
    )

  shouldTraverse(
    iri("x"),
    """|prefix : <http://example.org/>
       |:x :p _:1, _:2 ;
       |   :q  "Plain String" .
       |_:1 :p :y, :z .
       |:r :q :x .
      """.stripMargin,
    LazyList(
      iri("x"), iri("y"), iri("z"),
      bnode("1"), bnode("2"),
      string("Plain String")
    ),
    true
  )

  shouldTraverse(
    iri("x"),
    """|prefix : <http://example.org/>
       |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
       |:x :p _:1, _:2 ;
       |   :q  "<p>HTML String</p>"^^rdf:HTML .
       |_:1 :p :y, :z .
       |:r :q :x .
      """.stripMargin,
    LazyList(
      iri("x"), iri("y"), iri("z"),
      bnode("1"), bnode("2"),
      rdfHTML("<p>HTML String</p>")
    ),
    true
  )

    def shouldTraverse(node: RDFNode,
                       str: String,
                       expected: LazyList[RDFNode],
                       withLog: Boolean = false): Unit = {
      test(s"shouldTraverse(${node.show} in graph\n${str}\n and return\n$expected") {
        val r = RDFAsJenaModel.fromChars(str, "TURTLE", None).flatMap(res => res.use(rdf => for {
          _ <- showLog(rdf, "RDF", withLog)
          traversed <- stream2io(Graph.traverse(node, rdf))
          _ <- IO {
            pprint.log(traversed.toList)
          }
        } yield traversed))
        r.map(ls => assertEquals(ls.sorted, expected.sorted))
      }
    }

    shouldTraverseWithArcs(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:z :p :x, :y .
      """.stripMargin,
      List(
        RDFTriple(iri("x"), iri("p"), iri("y")),
        RDFTriple(iri("x"), iri("p"), iri("x"))
      )
    )

    shouldTraverseWithArcs(
        iri("x"),
        """|prefix : <http://example.org/>
           |:x :p :x, :y .
           |:y :q :r .
           |:z :p :x, :y .
      """.stripMargin,
        List(
          RDFTriple(iri("x"), iri("p"), iri("y")),
          RDFTriple(iri("x"), iri("p"), iri("x")),
          RDFTriple(iri("y"), iri("q"), iri("r"))
        )
      )

    def shouldTraverseWithArcs(node: RDFNode,
                               str: String,
                               expected: List[RDFTriple]): Unit = {
      test(s"shouldTraverseWithArcs(${node.show} in graph ${str}) and return $expected") {
        val r = RDFAsJenaModel.fromChars(str, "TURTLE", None).flatMap(_.use(rdf => for {
          triples <- stream2io(Graph.traverseWithArcs(node, rdf))
        } yield triples))
        r.map(triples => assertEquals(triples.toList.sorted,expected.sorted))
      }
    }

  def showLog(rdf: RDFReader, tag: String, withLog: Boolean): IO[Unit] = if (withLog)
    for {
      str <- rdf.serialize("N-TRIPLES")
      _ <- IO {
        pprint.log(str, tag)
      }
    } yield ()
  else IO(())
}
