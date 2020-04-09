package es.weso.rdf.jena

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.utils.IOUtils._

class RDFAsJenaModelTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe(s"From String") {

   it(s"Parses a date") {
    val ex = "http://example.org#"
    val str =s"""|@prefix : <${ex}> .
                 |:p :q "2019-04-08T00:00:00Z"^^xsd:dateTime .
                 |:q :q 1 .
                 |""".stripMargin
     val r = for {
      rdf <- RDFAsJenaModel.fromString(str, "TURTLE")
      str <- rdf.serialize("TURTLE")
     } yield str
     r.attempt.unsafeRunSync.fold(err => fail(s"Error parsing $err"), str => info(s"Serialized as\n$str"))
    }
  }
/*
  describe("Checking base") {
    // println(s"ShaclFolder file...${shaclFolderURI}")

    it("should be able to parse RDF with relative URIs and base") {
      //val emptyModel = ModelFactory.createDefaultModel
      // val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[Prefix, IRI] = Map(Prefix("") -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      val str =
        """|@prefix : <http://example.org#> .
                   |:a :b <c> .
                   |""".stripMargin
      val r = for {
       e <- RDFAsJenaModel.empty
       e1 <- e.addPrefixMap(pm)
       e2 <- e1.addBase(IRI("http://example.org/base/"))
       expected <- e2.addTriples(Set(RDFTriple(
           IRI("http://example.org#a"),
           IRI("http://example.org#b"),
           IRI("c")))
         )
       rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", Some(IRI("http://example.org/base/")))
      } yield (expected,rdf)
      r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"),
        values => {
          val (expected,rdf) = values
          shouldBeIsomorphic(expected.model, rdf.model)
        })
    }

/*    it("should be able to parse RDF with relative URIs") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      rdf.addTriples(Set(RDFTriple(
        IRI("a"),
        IRI("b"),
        IntegerLiteral(1))
      ))
      val str =
        """|<a> <b> 1 .
                   |""".stripMargin
      val m = ModelFactory.createDefaultModel
      RDFAsJenaModel.fromChars(str, "TURTLE", None) match {
        case Right(m2) => shouldBeIsomorphic(rdf.model, m2.model)
        case Left(e) => fail(s"Error $e\n$str")
      }
    } */
  }

  describe(s"Triples with subject") {
       val ex = "http://example.org#"
       val str =s"""|@prefix : <${ex}> .
                    |:p :q :q .
                    |:q :q 1 .
                    |""".stripMargin
       val p = IRI(s"${ex}p")
       val q = IRI(s"${ex}q")
       val one = IntegerLiteral(1,"1") // DatatypeLiteral("1",IRI("http://www.w3.org/2001/XMLSchema#integer"))// IntegerLiteral(1)
       triplesWithSubject(str,p, List(RDFTriple(p,q,q)))
       triplesWithSubject(str,q, List(RDFTriple(q,q,one)))
       triplesWithSubject(str,one, List())
     }

   def triplesWithSubject(strRdf: String, node: RDFNode, expected: List[RDFTriple]): Unit = {
     it(s"Should calculate triplesWithSubject($node) for ${strRdf} and get ${expected}") {
     val r = for {
       rdf <- RDFAsJenaModel.fromString(strRdf,"TURTLE")
       ts <- stream2io(rdf.triplesWithSubject(node))
     } yield ts
     r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), 
      ts => ts should contain theSameElementsAs(expected)
     )
    }
   }

  describe(s"Triples with object") {
       val ex = "http://example.org#"
       val str =s"""|@prefix : <${ex}> .
                    |:p :q :q .
                    |:q :q 1 .
                    |""".stripMargin
       val p = IRI(s"${ex}p")
       val q = IRI(s"${ex}q")
       val one = IntegerLiteral(1,"1") // DatatypeLiteral("1",IRI("http://www.w3.org/2001/XMLSchema#integer"))// IntegerLiteral(1)
       triplesWithObject(str,p, List())
       triplesWithObject(str,q, List(RDFTriple(p,q,q)))
       triplesWithObject(str,one, List(RDFTriple(q,q,one)))
     }

   def triplesWithObject(strRdf: String, node: RDFNode, expected: List[RDFTriple]): Unit = {
     it(s"Should calculate triplesWithObject($node) for ${strRdf} and get ${expected}") {
     val r = for {
       rdf <- RDFAsJenaModel.fromString(strRdf,"TURTLE")
       ts <- stream2io(rdf.triplesWithObject(node))
     } yield ts
     r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), 
      ts => ts should contain theSameElementsAs(expected)
     )
    }
   } */
}

