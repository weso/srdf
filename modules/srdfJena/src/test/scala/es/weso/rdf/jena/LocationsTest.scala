package es.weso.rdf.jena

import cats.effect._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.utils.IOUtils._
import cats.implicits._

class LocationsTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe(s"Get locations") {

   it(s"Locations of bNodes") {
    val ex = "http://example.org#"
    val str =s"""|@prefix : <${ex}> .
                 |@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                 |:p :q _:1 .
                 |_:1 :q 1 .
                 |""".stripMargin
     val r = for {
       r <- RDFAsJenaModel.fromString(str, "TURTLE")
       ps <- r.use(rdf => IO((rdf.nodesLocations, rdf.triplesLocations)))
     } yield ps
     r.attempt.unsafeRunSync().fold(
       err => fail(s"Error parsing $err"), 
       ms => {
         val (nls, tls) = ms
         info(nls.toString)
       }
     )
  }
}


}

