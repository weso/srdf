package es.weso.rdf.jena

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import cats.effect.IO
import fs2.Stream
import es.weso.rdf.nodes._

class RDFJenaSpec extends AnyFunSpec with JenaBased with Matchers with EitherValues {

  describe("Adding triples") {
    it("should be able to add a single triple with IRIs") {
      val map: Map[Prefix, IRI] = Map(Prefix("") -> IRI("http://example.org#"))
      val pm: PrefixMap         = PrefixMap(map)
      val m2                    = str2model("""|@prefix : <http://example.org#> .
                            |:a :b :c .
                            |""".stripMargin)

      val r = for {
        empty <- RDFAsJenaModel.empty
        rdf1  <- empty.addPrefixMap(pm)
        rdf2 <- rdf1.addTriples(
          Set(RDFTriple(IRI("http://example.org#a"), IRI("http://example.org#b"), IRI("http://example.org#c")))
        )
      } yield rdf2
      r.attempt.unsafeRunSync.fold(s => s"Error: ${s.getMessage()}", rdf => shouldBeIsomorphic(rdf.model, m2))
    }

    it("should be able to add some triples with BNodes") {
      val map: Map[Prefix, IRI] =
        Map(Prefix("") -> IRI("http://example.org#"), Prefix("foaf") -> IRI("http://foaf.org#"))
      val pm: PrefixMap = PrefixMap(map)
      val m2            = str2model("""|@prefix : <http://example.org#> .
                            |@prefix foaf: <http://foaf.org#> .
                            |:a foaf:knows _:x .
                            |_:x foaf:knows _:y .
                            |_:y foaf:name "pepe" .
                            |""".stripMargin)

      val r = for {
        empty <- RDFAsJenaModel.empty
        rdf1  <- empty.addPrefixMap(pm)
        rdf2 <- rdf1.addTriples(
          Set(
            RDFTriple(IRI("http://example.org#a"), IRI("http://foaf.org#knows"), BNode("b" + 1)),
            RDFTriple(BNode("b" + 1), IRI("http://foaf.org#knows"), BNode("b" + 2)),
            RDFTriple(BNode("b" + 2), IRI("http://foaf.org#name"), StringLiteral("pepe"))
          )
        )
      } yield rdf2
      r.attempt.unsafeRunSync.fold(s => s"Error: ${s.getMessage}", rdf => shouldBeIsomorphic(rdf.model, m2))
    }

  }

  describe("Parsing other formats") {
    it("Should be able to parse NTriples") {
      val m1                  = str2model("""|@prefix : <http://example.org#> .
                          |:a :b :c .
                          |""".stripMargin)
      val str_triples         = "<http://example.org#a> <http://example.org#b> <http://example.org#c> ."
      val rdf: RDFAsJenaModel = RDFAsJenaModel(ModelFactory.createDefaultModel())
      val rdf2                = rdf.fromString(str_triples, "NTRIPLES").unsafeRunSync
      val m2                  = RDFAsJenaModel.extractModel(rdf2)
      shouldBeIsomorphic(m1, m2)
    }
  }

  describe("Querying RDF graphs") {
    it("Should be able to get iriObjects of some type") {
      val str   = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin
      val typeC = IRI("http://example.org#C")
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithType(typeC).compile.toList
      } yield triples
      val a  = IRI("http://example.org#a")
      val b  = IRI("http://example.org#b")
      val t1 = RDFTriple(a, `rdf:type`, typeC)
      val t2 = RDFTriple(b, `rdf:type`, typeC)
      r.attempt.unsafeRunSync.fold(s => s"Error$s", triples => triples should contain theSameElementsAs (Set(t1, t2)))
    }

    it("Should be able to get subjects") {
      val str = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin

      val a     = IRI("http://example.org#a")
      val p     = IRI("http://example.org#p")
      val typeC = IRI("http://example.org#C")
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithSubject(a).compile.toList
      } yield triples
      val t1 = RDFTriple(a, `rdf:type`, typeC)
      val t2 = RDFTriple(a, p, IntegerLiteral(1, "1"))
      r.attempt.unsafeRunSync
        .fold(s => s"Error: ${s}", triples => triples should contain theSameElementsAs (Set(t1, t2)))
    }

    it("Should be able to get subjects with xsd:date") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :date "25/10/2015"^^xsd:date .
                   |""".stripMargin
      val a   = IRI("http://example.org#a")
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithSubject(a).compile.toList
      } yield triples
      val date  = IRI("http://example.org#date")
      val value = DatatypeLiteral("25/10/2015", IRI("http://www.w3.org/2001/XMLSchema#date"))
      val t1    = RDFTriple(a, date, value)
      r.attempt.unsafeRunSync
        .fold(s => s"Error: ${s.getMessage}", triples => triples should contain theSameElementsAs (Set(t1)))
    }

    it("Should be able to get subjects with xsd:integer") {
      val str   = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age 15 .
                   |""".stripMargin
      val a     = IRI("http://example.org#a")
      val age   = IRI("http://example.org#age")
      val value = IntegerLiteral(15, "15")
      val t1    = RDFTriple(a, age, value)
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithSubject(a).compile.toList
      } yield triples
      r.attempt.unsafeRunSync
        .fold(s => s"Error: ${s.getMessage}", triples => triples should contain theSameElementsAs (Set(t1)))
    }

    it("Should be able to get subjects with datatype :xxx") {
      val str   = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "15"^^:xxx .
                   |""".stripMargin
      val a     = IRI("http://example.org#a")
      val age   = IRI("http://example.org#age")
      val value = DatatypeLiteral("15", IRI("http://example.org#xxx"))
      val t1    = RDFTriple(a, age, value)
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithSubject(a).compile.toList
      } yield triples
      r.attempt.unsafeRunSync.fold(s => s"Error: ${s.getMessage()}", _ should contain theSameElementsAs (Set(t1)))
    }

    it("Should be able to get subjects with lang literal") {
      val str   = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "hi"@en .
                   |""".stripMargin
      val a     = IRI("http://example.org#a")
      val age   = IRI("http://example.org#age")
      val value = LangLiteral("hi", Lang("en"))
      val t1    = RDFTriple(a, age, value)
      val r = for {
        rdf     <- RDFAsJenaModel.fromString(str, "TURTLE")
        triples <- rdf.triplesWithSubject(a).compile.toList
      } yield triples
      r.attempt.unsafeRunSync.fold(s => s"Error: ${s.getMessage()}", _ should contain theSameElementsAs (Set(t1)))
    }

  }

  describe("hasClass") {
    it("check hasClass") {
      val ex                 = "http://example.org"
      val rdfStr             = s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin
      val e                  = IRI(ex)
      val person1            = e + "person1"
      val teacher1           = e + "teacher1"
      val teacher2           = e + "teacher2"
      val dog1               = e + "dog1"
      val any                = e + "any"
      val _Person            = e + "Person"
      val _Teacher           = e + "Teacher"
      val _UniversityTeacher = e + "UniversityTeacher"
      val _Dog               = e + "Dog"
      val _Any               = e + "Any"

      val r = for {
        model <- RDFAsJenaModel.fromString(rdfStr, "TURTLE")
        _     <- check(model.hasSHACLClass(person1, _Person), true, "person1 a Person")
        _     <- check(model.hasSHACLClass(person1, _Teacher), false, "person1 a Teacher")
        _     <- check(model.hasSHACLClass(person1, _UniversityTeacher), false, "person1 a UniversityTeacher")
        _     <- check(model.hasSHACLClass(person1, _Dog), false, "person1 a Dog")
        _     <- check(model.hasSHACLClass(teacher1, _Person), true, "teacher1 a Person")
        _     <- check(model.hasSHACLClass(teacher1, _Teacher), true, "teacher1 a Teacher")
        _     <- check(model.hasSHACLClass(teacher1, _UniversityTeacher), false, "teacher1 a UniversityTeacher")
        _     <- check(model.hasSHACLClass(teacher1, _Dog), false, "teacher1 a Dog")
        _     <- check(model.hasSHACLClass(teacher2, _Person), true, "teacher2 a Person")
        _     <- check(model.hasSHACLClass(teacher2, _Teacher), true, "teacher2 a Teacher")
        _     <- check(model.hasSHACLClass(teacher2, _UniversityTeacher), true, "teacher2 a UnivTeacher")
        _     <- check(model.hasSHACLClass(teacher2, _Dog), false, "teacher2 a Dog")
        _     <- check(model.hasSHACLClass(dog1, _Person), false, "dog1 a Person")
        _     <- check(model.hasSHACLClass(dog1, _Teacher), false, "dog1 a Teacher")
        _     <- check(model.hasSHACLClass(dog1, _UniversityTeacher), false, "dog1 a UNivTeacher")
        _     <- check(model.hasSHACLClass(dog1, _Dog), true, "dog1 a Dog")
        _     <- check(model.hasSHACLClass(any, _Dog), false, "any a Dog")
        _     <- check(model.hasSHACLClass(any, _Any), false, "any a Any")
        _     <- check(model.hasSHACLClass(dog1, _Any), false, "dog1 a Any")
      } yield (())
      r.attempt.unsafeRunSync.fold(s => fail(s"Error: ${s.getMessage}"), _ => info(s"End"))

    }
  }

  describe("getSHACLInstances") {
    it("getSHACLInstances") {
      val ex                 = "http://example.org"
      val rdfStr             = s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin
      val e                  = IRI(ex)
      val person1            = e + "person1"
      val teacher1           = e + "teacher1"
      val teacher2           = e + "teacher2"
      val dog1               = e + "dog1"
      val _Person            = e + "Person"
      val _Teacher           = e + "Teacher"
      val _UniversityTeacher = e + "UniversityTeacher"
      val _Dog               = e + "Dog"
      val _Any               = e + "Any"

     implicit val rdfNodeOrdering = new Ordering[RDFNode] {
        def compare(a: RDFNode, b: RDFNode) = a.getLexicalForm compare b.getLexicalForm
     }

     val r = for {
        model <- RDFAsJenaModel.fromString(rdfStr, "TURTLE")
        _ <- checkLs(model.getSHACLInstances(_Person), List(person1, teacher1, teacher2), "person <- person1, teacher1, teacher2")
        _ <- checkLs(model.getSHACLInstances(_Teacher), List(teacher1, teacher2), "teacher <- teacher1 teacher2")
        _ <- checkLs(model.getSHACLInstances(_UniversityTeacher), List(teacher2), "univTeacher <- teacher2")
        _ <- checkLs(model.getSHACLInstances(_Dog), List(dog1),"Dog <- dog1")
        _ <- checkLs(model.getSHACLInstances(_Any), List(),"Any <- none")
      } yield (())
      r.attempt.unsafeRunSync.fold(s => fail(s"Error: ${s.getMessage}"), _ => info(s"End"))
    }
  }

  private def check[A](action: IO[A], expected: A, name: String): IO[Unit] =
    for {
      v <- action
      r <- if (v == expected) {
        info(s"${name}/${v}==${expected} as expected")
        IO.pure(())
      } else {
        IO.raiseError(new RuntimeException(s"${name}/${v}!=${expected}"))
      }
    } yield r

  private def checkLs[A: Ordering](action: Stream[IO,A], expected: List[A], name: String): IO[Unit] = {
    for {
      vs <- action.compile.toList
      r <- if (vs.sorted == expected.sorted) {
        info(s"${name}/${vs}==${expected} as expected")
        IO.pure(())
      } else 
      IO.raiseError(new RuntimeException(s"${name} failed!. Elements of lists different: \nObtained: ${vs.sorted.map(_.toString).mkString(",")}\nExpected:${expected.sorted.map(_.toString).mkString(",")}\n"))
    } yield r
  }
}
