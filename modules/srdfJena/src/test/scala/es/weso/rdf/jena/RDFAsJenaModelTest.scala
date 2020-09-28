package es.weso.rdf.jena

import cats.effect._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.utils.IOUtils._
import cats.implicits._

class RDFAsJenaModelTest
  extends AnyFunSpec
  with JenaBased
  with Matchers {

  describe(s"From String") {

   it(s"Parses a date") {
    val ex = "http://example.org#"
    val str =s"""|@prefix : <${ex}> .
                 |@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                 |:p :q "2019-04-08T00:00:00Z"^^xsd:dateTime .
                 |:q :q 1 .
                 |""".stripMargin
     val r = RDFAsJenaModel.fromString(str, "TURTLE").use(_.serialize("TURTLE"))
     r.attempt.unsafeRunSync.fold(err => fail(s"Error parsing $err"), str => info(s"Serialized as\n$str"))
  }

  it(s"Parses a negative number") {
    val ex = "http://example.org#"
    val str =s"""|@prefix : <${ex}> .
                 |@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                 |:p :q "-1"^^xsd:integer .
                 |:r :q -1 .
                 |""".stripMargin
     val r = RDFAsJenaModel.fromString(str, "TURTLE").use(
       _.serialize("TURTLE")
     )
     r.attempt.unsafeRunSync.fold(err => fail(s"Error parsing $err"), str => info(s"Serialized as\n$str"))
  }

  }

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

      val resources = for {
        e <- RDFAsJenaModel.empty
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", Some(IRI("http://example.org/base/")))
      } yield (e, rdf)

      val r: IO[Unit] = resources.use(pair => {
        val (e, rdf) = pair
        for {
          e1 <- e.addPrefixMap(pm)
          e2 <- e1.addBase(IRI("http://example.org/base/"))
          expected <- e2.addTriples(Set(RDFTriple(
            IRI("http://example.org#a"),
            IRI("http://example.org#b"),
            IRI("c")))
          )
          check <- fromES(checkIsomorphic(expected.model, rdf.model))
        } yield check
      })
      r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"),
        _ => info(s"Models are isomorphic"))
    }
    it("should be able to parse RDF with relative URIs") {
      val str =
        """|<a> <b> 1 .
           |""".stripMargin
      val ex = "http://example.org/"
      val r =
        ( RDFAsJenaModel.empty,
          RDFAsJenaModel.fromChars(str, "TURTLE", Some(IRI(ex)))
        ).tupled.use {
        case (rdf,rdf2) => for {
          rdf1 <- rdf.addTriples(Set(RDFTriple(IRI(ex + "a"),IRI(ex + "b"),IntegerLiteral(1))))
          _ <- fromES(checkIsomorphic(rdf1.model,rdf2.model))
        } yield ()
      }

      // val m = ModelFactory.createDefaultModel
      r.attempt.unsafeRunSync.fold(
        s => s"Error: ${s.getMessage}",
        pair => info(s"Models are isomorphic")
      ) 
    }

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
     val r = RDFAsJenaModel.fromString(strRdf,"TURTLE").use(
       rdf => for {
           ts <- stream2io(rdf.triplesWithSubject(node))
         } yield ts
     )
     r.attempt.unsafeRunSync.fold(
       e => fail(s"Error: $e"),
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
     val r = RDFAsJenaModel.fromString(strRdf,"TURTLE").use(rdf => for {
       ts <- stream2io(rdf.triplesWithObject(node))
     } yield ts)
     r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), 
      ts => ts should contain theSameElementsAs(expected)
     )
    }
  }

  describe(s"Merging without keeping BNode labels") {
    val str1 = """|prefix : <http://example.org/>
                 |:x :p _:1 .
                 |_:1 :q :r .
                 |""".stripMargin 
    val str2 = """|prefix : <http://example.org/>
                  |:x :p _:1 .
                  |_:1 :q :r .
                  |""".stripMargin 
    it(s"Merges with BNodes") {
      val resources: Resource[IO, (RDFAsJenaModel, RDFAsJenaModel)] = for {
        rdf1 <- RDFAsJenaModel.fromString(str1, "TURTLE", None, false)
        rdf2 <- RDFAsJenaModel.fromString(str2, "TURTLE", None, false)
      } yield (rdf1,rdf2)
      val r = resources.use(pair => {
        val (rdf1, rdf2) = pair
        val merged = RDFAsJenaModel(rdf1.model.add(rdf2.model))
        merged.serialize("TURTLE")
      })
      r.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        str => info(s"Merged: $str")
      )
    }
  }

  describe(s"Merging keeping BNode labels") {
    val str1 = """|prefix : <http://example.org/>
                 |:x :p _:1 .
                 |_:1 :q :r .
                 |""".stripMargin 
    val str2 = """|prefix : <http://example.org/>
                  |:x :p _:1 .
                  |_:1 :q :r .
                  |""".stripMargin 
    it(s"Merges with BNodes") {
      val resources = for {
        rdf1 <- RDFAsJenaModel.fromString(str1,"TURTLE", None,true)
        rdf2 <- RDFAsJenaModel.fromString(str2,"TURTLE", None,true)
      } yield (rdf1,rdf2)
      val r = resources.use(pair => {
        val (rdf1, rdf2) = pair
        val merged = RDFAsJenaModel(rdf1.model.add(rdf2.model))
        merged.serialize("TURTLE")
      })
      r.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        str => info(s"Merged: $str"))
    }
  }

}

