package es.weso.rdf.jena

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf._
// import es.weso.utils.IOUtils._
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES._
import scala.concurrent.ExecutionContext.global
import org.http4s.client.dsl.io._
import org.http4s.client.blaze._
import cats.effect._
import org.http4s.implicits._
import cats._
import cats.implicits._

class RDFFromWebTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe(s"From Wikidata item") {
    implicit val cs: ContextShift[IO] = IO.contextShift(global)
    implicit val showRDFTriple: Show[RDFTriple] = new Show[RDFTriple] {
      def show(triple: RDFTriple): String = s"[${triple.subj.show},${triple.pred.show},${triple.obj.show}]"
    }
    implicit val timer: Timer[IO] = IO.timer(global)

    val wd = IRI("http://www.wikidata.org/entity/")
    val wdt = IRI("http://www.wikidata.org/prop/direct/")
    val wikidataPrefixMap = PrefixMap(Map(
        Prefix("wd") -> wd,
        Prefix("wdt") -> wdt
      ))

    // TODO: We ignore the following test to avoid tests that depend on internet access
    ignore("Gets labels") {
      val r = BlazeClientBuilder[IO](global).resource.use { client => {
       val web = RDFFromWeb(Some(wikidataPrefixMap), Some(client))
       web.triplesWithSubjectPredicate(wd + "Q42", rdfs + "label").compile.toList
      }
     }

    r.attempt.unsafeRunSync.fold(s => s"Error: $s", ts => { 
      info(s"Labels: ${ts.length}") 
      ts.length should be > 0
     })
   }
  }
}

