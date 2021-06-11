package es.weso.rdf.jena

import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.apache.jena.query._
import cats.effect._

class SPARQLQueriesTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe("SPARQL queries test") {
    it(s"Should run a query") {
      // val sp = PredicatePath(IRI("http://example.org/p"))
      // val query = SPARQLQueries.queryPath(sp)
      /*shouldQuery(query,
      s"""|prefix : <http://example.irg/>
          |:x :p :y .""".stripMargin, 
      ""
      )*/
    }

    ignore("Should create SPARQL query from an inverse SHACLPath") {
      val sp = InversePath(PredicatePath(IRI("http://example.org/p")))
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }

    ignore("Should create SPARQL query from a sequence SHACLPath") {
      val sp = SequencePath(Seq(
        PredicatePath(IRI("http://example.org/p")),
        PredicatePath(IRI("http://example.org/q")))
      )
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }
 }

 def shouldQuery(query: Query, rdfStr: String, expected: String): Unit = {
   ignore(s"Should query $query on $rdfStr and obtain $expected") {
   val queryExecution: IO[Resource[IO,QueryExecution]] =
     IO(Resource.make(IO { QueryExecutionFactory.create(query) })(qe => IO(qe.close())))
   val r: IO[Unit] = for {
     res1 <- RDFAsJenaModel.fromString(rdfStr, "TURTLE")
     res2 <- queryExecution
    v <- (res1,res2).tupled.use {
       _ => IO(())
     }
    } yield v
   r.attempt.unsafeRunSync().fold(e => fail(s"Error: $e"), v => info(s"$v"))
  }
 }

}