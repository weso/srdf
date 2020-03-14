package es.weso.rdf.jena

import cats.effect._
import java.nio.file.Paths
import es.weso.utils.IOUtils._
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.nodes._
import org.scalatest.{EitherValues, FunSpec, Matchers}

class ImportsTest extends FunSpec with JenaBased with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val rdfFolderStr = conf.getString("rdfFolder")
  val rdfFolder = IRI(Paths.get(rdfFolderStr).normalize.toUri.toString)

  describe("Merge test") {

    it(s"Should read merged file") {
    val r = io2ES(for {
      rdf1 <- RDFAsJenaModel.fromIRI(rdfFolder + "/merged.ttl")
      iris <- rdf1.iris.compile.toList
    } yield (rdf1,iris))

    r.fold(e => fail(s"Error: $e"), t => {
      val (rdf,iris) = t
      info(s"RDF read: ${iris}")
      iris.size should be(1)
    })
    } 

    it(s"Should read m1") {
      val r = io2ES(for {
        rdf1 <- RDFAsJenaModel.fromIRI(rdfFolder + "/m1.ttl")
        iris <- rdf1.iris.compile.toList
      } yield (rdf1,iris))
  
      r.fold(e => fail(s"Error: $e"), t => {
        val (rdf,iris) = t
        info(s"IRIs from m1: ${iris}")
        iris.size should be(1)
      })
      } 
  

    it(s"Should merge files") {
      val r = io2ES(for {
        rdf1 <- RDFAsJenaModel.fromIRI(rdfFolder + "/m1.ttl")
        srdf1 <- rdf1.serialize("N-TRIPLES")
        _ <- IO { println(s"rdf1:\n${srdf1}") }
        rdf2 <- RDFAsJenaModel.fromIRI(rdfFolder + "/m2.ttl")
        srdf2 <- rdf2.serialize("N-TRIPLES")
        _ <- IO { println(s"rdf2:\n${srdf2}") }
        merged <- rdf1.merge(rdf2)
        smerged <- merged.serialize("N-TRIPLES")
        _ <- IO { println(s"merged:\n${smerged}") }
        mergedFromFile <- RDFAsJenaModel.fromIRI(rdfFolder + "/mergedWithoutRenaming.ttl")
        smergedFromFile <- mergedFromFile.serialize("N-TRIPLES")
        _ <- IO { println(s"mergedExpected:\n${smergedFromFile}") }
        b <- merged.isIsomorphicWith(mergedFromFile)
      } yield {
        (merged,mergedFromFile,b)
      })

      r.fold(e => fail(s"Error: $e"), values => {
        val (_, _,b) = values
        b should be(true)
      })
    }
  }

/*  describe("Imports test") {
    it(s"Should extend with imports") {
    
     val r = io2ES(for {
       rdf1     <- RDFAsJenaModel.fromIRI(rdfFolder + "/testImport.ttl")
       extended <- rdf1.extendImports
       x = IRI("http://example.org/x")
       p = IRI("http://example.org/p")
       ts <- extended.triplesWithSubjectPredicate(x,p).compile.toList
      } yield (rdf1,extended,ts)
     )

      r.fold(e => fail(s"Error: $e"), values => {
        val (_,_,ts) = values
        ts.size should be(2)
      })
    }

    it(s"Should handle loops") {
      val r = io2ES(for {
        rdf1     <- RDFAsJenaModel.fromIRI(rdfFolder + "/testImportWithLoop.ttl")
        extended <- rdf1.extendImports()
        ts <- rdf1.rdfTriples.compile.toList
        tse <- extended.rdfTriples.compile.toList
      } yield (rdf1,extended,ts,tse)
      )

      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf1,extended,ts,tse) = values
        ts.size should be(tse.size)
      })
    }

  } */
}

