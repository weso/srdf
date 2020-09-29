package es.weso.rdf.parser
import org.scalatest._
import util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import cats.data.EitherT
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import cats.effect._

class RDFParserTest extends AnyFunSpec with Matchers with RDFParser with EitherValues {

  describe("RDFParser") {

    describe("iriFromPredicate") {
      it("iriFromPredicate simple") {
        val cs =
          """|prefix : <http://example.org/>
             |:x :p :T .""".stripMargin
        val n: RDFNode = IRI("http://example.org/x")
        val p: IRI = IRI("http://example.org/p")
        val r: IO[IRI] = RDFAsJenaModel.fromString(cs, "TURTLE").use(rdf => for {
          eitherIri <- iriFromPredicate(p).value.run(Config(n, rdf))
          iri <- eitherIri.fold(IO.raiseError[IRI](_), IO.pure(_))
        } yield iri
        )
        r.attempt.unsafeRunSync.fold(
          e => fail(s"Error: $e"),
          iri => iri should be(IRI("http://example.org/T"))
        )
      }

      it("iriFromPredicate fails when more than one matches") {
        val cs =
          """|prefix : <http://example.org/>
             |:x :p :T, :S .""".stripMargin
        val try1 = RDFAsJenaModel.fromString(cs, "TURTLE").use(rdf => {
          val n: RDFNode = IRI("http://example.org/x")
          val p: IRI = IRI("http://example.org/p")
          iriFromPredicate(p).value.run(Config(n, rdf))
        })
        try1.unsafeRunSync.fold(
          e => e.getMessage should include("More than one value from predicate"),
          v => fail(s"Parsed as $v when it should fail")
        )
      }

      it("iriFromPredicate fails when no predicate") {
        val cs =
          """|prefix : <http://example.org/>
             |:x :p :T .""".stripMargin
        val n: RDFNode = IRI("http://example.org/x")
        val p: IRI = IRI("http://example.org/q")
        val try1 = RDFAsJenaModel.fromString(cs, "TURTLE").use(rdf => for {
          obj <- iriFromPredicate(p).value.run(Config(n, rdf))
        } yield obj
        )
        try1.unsafeRunSync.fold(
          s => s.getMessage should include("Not found triples with subject"),
          v => fail(s"Parsed as $v when it should fail"))
      }

    }

    describe("rdfType") {
      it("rdfType simple") {
        val cs =
          """|prefix : <http://example.org/>
             |:x a :T .""".stripMargin
        val n: RDFNode = IRI("http://example.org/x")
        val try1 = RDFAsJenaModel.fromString(cs, "TURTLE").use(rdf => for {
          obj <- rdfType.value.run(Config(n, rdf))
        } yield obj
        )
        try1.unsafeRunSync.fold(
          e => fail(s"Error: $e"),
          v => v should be(IRI("http://example.org/T")))
      }
    }
    it("rdfType fails when more than one type") {
      val cs =
        """|prefix : <http://example.org/>
           |:x a :T, :S .""".stripMargin

      val n: RDFNode = IRI("http://example.org/x")
      val try1 = RDFAsJenaModel.fromString(cs, "TURTLE").use(
        rdf => rdfType.value.run(Config(n, rdf))
      )
      try1.unsafeRunSync.fold(
        s => s.getMessage should include("More than one value"),
        v => fail(s"Parsed as $v when it should fail")
      )
    }
  }

  /*
      it("rdfType fails when no type") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/q")
          obj <- EitherT(rdfType.value.run(Config(n,rdf)))
        } yield (obj)
        try1.value.unsafeRunSync match {
          case Left(s) => s should include("Not found triples")
          case Right(v) => fail(s"Parsed as $v when it should fail")
        }
      }

    }

    describe(
      "rdfList") {

        it("rdfList happy path") {
          val cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- EitherT(objectFromPredicate(p).value.run(Config(n,rdf)))
            ls <- EitherT(rdfList.value.run(Config(nodeLs,rdf)))
          } yield (ls)
          try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), 
             v => v should be(List(IntegerLiteral(1,"1"), IntegerLiteral(2,"2"), IntegerLiteral(3,"3")))
             )
        }

        it("rdfList empty") {
          val cs = """|prefix : <http://example.org/>
                  |:x :p () .""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- EitherT(objectFromPredicate(p).value.run(Config(n,rdf)))
            ls <- EitherT(rdfList.value.run(Config(nodeLs,rdf)))
          } yield (ls)
          try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), v => v should be(List()))
        }

        it("rdfList with rdf:first and rdf:rest") {
          val cs = """|prefix : <http://example.org/>
                  |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                  |:x :p :e1 .
                  |:e1 rdf:first 1; rdf:rest :e2 .
                  |:e2 rdf:first 2; rdf:rest rdf:nil .
                  |""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- EitherT(objectFromPredicate(p).value.run(Config(n,rdf)))
            ls <- EitherT(rdfList.value.run(Config(nodeLs,rdf)))
          } yield (ls)
          try1.value.unsafeRunSync.fold(e => fail(s"Error: ${e}"), 
             value => value should be(List(IntegerLiteral(1,"1"), IntegerLiteral(2,"2")))
          )
        }
        it("rdfList with circular structure") {
          val cs = """|prefix : <http://example.org/>
                  |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                  |:x :p :e1 .
                  |:e1 rdf:first 1; rdf:rest :e2 .
                  |:e2 rdf:first 2; rdf:rest :e1 .
                  |""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- EitherT(objectFromPredicate(p).value.run(Config(n,rdf)))
            ls <- EitherT(rdfList.value.run(Config(nodeLs,rdf)))
          } yield (ls)
          try1.value.unsafeRunSync.fold(e => info(s"Error as expected: ${e}"), 
             value => fail(s"Parsed without errors when an error should be raised: $value")
          )
        }

      }

    describe("rdfListForPredicate") {
      it("rdfListForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .
                  |""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          ls <- EitherT(rdfListForPredicate(p).value.run(Config(n,rdf)))
        } yield (ls)
        try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), v => v should be(List(IntegerLiteral(1,"1"), IntegerLiteral(2,"2"), IntegerLiteral(3,"3"))))
      }
    }

    describe("integerLiteralForPredicate") {

      it("integerLiteralForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p 1 .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          n <- EitherT(integerLiteralForPredicate(p).value.run(Config(n,rdf)))
        } yield (n)
        try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), v => v should be(1))
      }
    }

    describe("anyOf") {

      it("anyOf when some one is ok") {
        val cs = """|prefix : <http://example.org/>
                    |:x :p :y .""".stripMargin
        val y = IRI("http://example.org/y")
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          q = IRI("http://example.org/q")
          n <- EitherT(anyOf(objectFromPredicate(p), objectFromPredicate(q)).value.run(Config(n,rdf)))
        } yield (n)
        try1.value.unsafeRunSync match {
          case Left(e) => fail(s"Error: $e")
          case Right(value) => value should contain only (y)
        }
      }

      it(
        "anyOf when some all fail should fail") {
          val cs =
            """|prefix : <http://example.org/>
                  |:x :p :y .""".stripMargin
          val q = IRI("http://example.org/q")
          val r = IRI("http://example.org/r")
          val try1 = for {
            rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            n <- EitherT(anyOf(
              objectFromPredicate(q),
              objectFromPredicate(r)).value.run(Config(n,rdf)))
          } yield (n)
          try1.value.unsafeRunSync match {
            case Left(e) => fail(s"Failed with $e")
            case Right(values) => values shouldBe empty
          }
        }
    }

    describe("arc") {

      it("parses single arc") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                   |:x :p () .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, rdfNil).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => fail(s"Failed with $e")
          case Right(value) => value shouldBe List()
        }
      }
    }
    describe("list1Plus") {

      it("parses list1Plus ") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y :z) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, list1Plus(condition(isIri, "isIRI"))).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => fail(s"Failed with $e")
          case Right(values) => values should contain theSameElementsAs List(y, z)
        }
      }

      it("fails list1Plus with recursive nodes in RDF list") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |:x :p _:1 .
                     |_:1 rdf:first :y .
                     |_:1 rdf:rest _:1 .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, list1Plus(condition(isIri, "isIRI"))).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => info("fails as expected")
          case Right(values) => fail(s"Fails because it obtained values $values but should have failed")
        }
      }

    }

    describe("list2Plus") {
      it("parses list2Plus ") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y :z) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, list2Plus(condition(isIri, "isIRI"))).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => fail(s"Failed with $e")
          case Right(values) => values should contain theSameElementsAs List(y, z)
        }
      }

      it("fails list2Plus if only one") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y ) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, list2Plus(condition(isIri, "isIRI"))).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => info(s"Failed as expected $e")
          case Right(values) => fail(s"Should fail if only one value but succedded with $values")
        }
      }

      it("fails list2Plus with recursive nodes in RDF list") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |:x :p _:1 .
                     |_:1 rdf:first :y .
                     |_:1 rdf:rest _:1 .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(str, "TURTLE")
          v <- EitherT(arc(p, list2Plus(condition(isIri, "isIRI"))).value.run(Config(x,rdf)))
        } yield (v)
        try1.value.unsafeRunSync match {
          case Left(e) => info("fails as expected")
          case Right(values) => fail(s"Fails because it obtained values $values but should have failed")
        }
      }
    } 
    describe("literalsFromPredicate") {
      it("literals from predicate") {
        val cs = """|prefix : <http://example.org/>
                   |:x :p 3 .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/p")
          obj <- EitherT(literalsFromPredicate(p).value.run(Config(n,rdf)))
        } yield (obj)
        try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), 
             v => v should be(List(IntegerLiteral(3,"3"))))
      }
    }

    describe("firstOf") {

      it("firstOf with inheritance") {
        val cs = """|prefix : <http://example.org/>
                   |:x :p 3 .""".stripMargin
        val p: IRI = IRI("http://example.org/p")
        val n: RDFNode = IRI("http://example.org/x")
        trait Base
        case class I(n: Int) extends Base
        case class S(n: String) extends Base
        def i(): RDFParser[Base] = for {
         obj <- objectFromPredicate(p)
         n <- withNode(obj,integer)   
        } yield I(n)
        def s(): RDFParser[Base] = for {
          obj <- objectFromPredicate(p)
          n <- withNode(obj,string)   
         } yield S(n)
        def fo: RDFParser[Base] =
         firstOf(i,s) 
 
        val try1 = for {
          rdf <- RDFAsJenaModel.fromStringIO(cs, "TURTLE")
          obj <- EitherT(fo.value.run(Config(n,rdf)))
        } yield (obj)
        try1.value.unsafeRunSync.fold(e => fail(s"Error: $e"), 
             v => v should be(I(3)))
      }
    }
  } */

}
