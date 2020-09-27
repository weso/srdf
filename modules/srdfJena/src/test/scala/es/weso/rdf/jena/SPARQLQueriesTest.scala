package es.weso.rdf.jena

import es.weso.rdf._
import cats.data._ 
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.apache.jena.query._
import es.weso.utils.IOUtils._
import cats.effect._

class SPARQLQueriesTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe("SPARQL queries test") {

    {
      val sp = PredicatePath(IRI("http://example.org/p"))
      val query = SPARQLQueries.queryPath(sp)
      shouldQuery(query, 
      s"""|prefix : <http://example.irg/>
          |:x :p :y .""".stripMargin, 
      ""
      )
    }

    it("Should create SPARQL query from a inverse SHACLPath") {
      val sp = InversePath(PredicatePath(IRI("http://example.org/p")))
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }

    it("Should create SPARQL query from a sequence SHACLPath") {
      val sp = SequencePath(Seq(
        PredicatePath(IRI("http://example.org/p")),
        PredicatePath(IRI("http://example.org/q")))
      )
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }
 }

 def shouldQuery(query: Query, rdfStr: String, expected: String): Unit = {
   it(s"Should query $query on $rdfStr and obtain $expected") {
   val r: IO[Unit] = RDFAsJenaModel.fromString(rdfStr, "TURTLE").use(rdf =>
     IO { QueryExecutionFactory.create(query) })
   r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), v => info(s"$v"))
  }
 }

}