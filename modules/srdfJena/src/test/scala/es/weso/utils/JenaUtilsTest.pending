package es.weso.utils

import es.weso.rdf.jena.JenaMapper
import es.weso.rdf.nodes.IRI
import es.weso.rdf.path.PredicatePath
import org.apache.jena.sparql.path.{Path, P_Link, P_OneOrMoreN}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import cats._
import cats.data._
import cats.effect._
import es.weso.utils.IOUtils._
import org.apache.jena.rdf.model.RDFNode
// import org.apache.jena.vocabulary.RDF
import org.apache.jena.rdf.model.Model


class JenaUtilsTest extends AnyFunSpec with Matchers with EitherValues {

  describe("hasClass") {

    it("check hasClass") {
      val ex = "http://example.org/"
      val rdfStr =s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin


      MonadError[IO,Throwable].attempt(JenaUtils.parseFromString(rdfStr)).unsafeRunSync match {
        case Right(model) => {
          val person1 = model.createResource(ex + "person1")
          val teacher1 = model.createResource(ex + "teacher1")
          val teacher2 = model.createResource(ex + "teacher2")
          val dog1 = model.createResource(ex + "dog1")
          val any = model.createResource(ex + "any")
          val _Person = model.createResource(ex + "Person")
          val _Teacher = model.createResource(ex + "Teacher")
          val _UniversityTeacher = model.createResource(ex + "UniversityTeacher")
          val _Dog = model.createResource(ex + "Dog")
          val _Any = model.createResource(ex + "Any")

          JenaUtils.hasClass(person1, _Person, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(person1, _Teacher, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(person1, _UniversityTeacher, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(person1, _Dog, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(teacher1, _Person, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(teacher1, _Teacher, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(teacher1, _UniversityTeacher, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(teacher1, _Dog, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(teacher2, _Person, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(teacher2, _Teacher, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(teacher2, _UniversityTeacher, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(teacher2, _Dog, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(dog1, _Person, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(dog1, _Teacher, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(dog1, _UniversityTeacher, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(dog1, _Dog, model).unsafeRunSync should be(true)
          JenaUtils.hasClass(any, _Dog, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(any, _Any, model).unsafeRunSync should be(false)
          JenaUtils.hasClass(dog1, _Any, model).unsafeRunSync should be(false)
        }
        case Left(msg) => fail(msg)
      }
    }

    describe(s"Check hasClass with blank nodes") {
      val rdfStr =
        s"""|@prefix : <http://example.org/>
            |_:x a :A, :B, _:C ;
            |    :x 1 . # To identify it as _:x
            |_:y a :B, _:C ;
            |    :y 1 .
            |:z a :B .
            |_:C :c 1 .
         """.stripMargin

      val r: EitherT[IO, String, Unit] = for {
        model <- io2esf[Model,IO](JenaUtils.parseFromString(rdfStr))
        ex <- either2es(IRI.fromString("http://example.org/"))
        px <- io2esf[Path,IO](JenaMapper.path2JenaPath(PredicatePath(ex + "x"), model, None))
        py <- io2esf[Path,IO](JenaMapper.path2JenaPath(PredicatePath(ex + "y"), model, None))
        pc <- io2esf[Path,IO](JenaMapper.path2JenaPath(PredicatePath(ex + "c"), model, None))
        bx <- io2esf[RDFNode,IO](JenaUtils.getNodesFromPath(px, model).map(_.head._1))
        by <- io2esf[RDFNode,IO](JenaUtils.getNodesFromPath(py, model).map(_.head._1))
        a = JenaMapper.rdfNode2JenaNode(ex+"A", model, None)
        b = JenaMapper.rdfNode2JenaNode(ex+"B", model, None)
        z = JenaMapper.rdfNode2JenaNode(ex+"z", model, None)
        bc <- io2esf[RDFNode,IO](JenaUtils.getNodesFromPath(pc, model).map(_.head._1))
        _ <- io2esf[Unit,IO](checkHasClass(bx,a,model, true))
        _ <- io2esf[Unit,IO](checkHasClass(bx,b,model, true))
        _ <- io2esf[Unit,IO](checkHasClass(by,a,model, false))
        _ <- io2esf[Unit,IO](checkHasClass(by,b,model, true))
        _ <- io2esf[Unit,IO](checkHasClass(bx,bc,model, true))
        _ <- io2esf[Unit,IO](checkHasClass(by,bc,model, true))
        _ <- io2esf[Unit,IO](checkHasClass(z,bc,model, false))
       } yield (())
      run_es(r).unsafeRunSync.fold(s => fail(s"Error"), v => info(s"OK"))
    }

    def checkHasClass(node: RDFNode, cls: RDFNode, model: Model, expected: Boolean): IO[Unit] = {
      JenaUtils.hasClass(node,cls,model).flatMap(b => IO { 
        it(s"$node hasClass $cls should be $expected") {
          b should be(expected) 
      }
      })
    }
    
  }

  describe(s"HasSHACLInstances") {
    describe(s"hasSHACLinstances should obtain SHACL instances") {
      val ex = "http://example.org/"
      val rdfStr = s"""|@prefix : <$ex> .
                       |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                       |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
                       |:person1 a :Person .
                       |:teacher1 a :Teacher .
                       |:teacher2 a :UniversityTeacher .
                       |:Teacher rdfs:subClassOf :Person .
                       |:UniversityTeacher rdfs:subClassOf :Teacher .
                       |:dog1 a :Dog .""".stripMargin

      val r: EitherT[IO,String,Unit] = for {
        model <- io2esf[Model,IO](JenaUtils.parseFromString(rdfStr))
        person1 = model.createResource(ex + "person1")
        teacher1 = model.createResource(ex + "teacher1")
        teacher2 = model.createResource(ex + "teacher2")
        dog1 = model.createResource(ex + "dog1")
        _Person = model.createResource(ex + "Person")
        _Teacher = model.createResource(ex + "Teacher")
        _UniversityTeacher = model.createResource(ex + "UniversityTeacher")
        _Dog = model.createResource(ex + "Dog")
        _Any = model.createResource(ex +"Any")
        _ <- io2esf[Unit,IO](checkGetSHACLInstances(_Person, model,List(person1, teacher1, teacher2)))
        _ <- io2esf[Unit,IO](checkGetSHACLInstances(_Teacher, model,List(teacher1, teacher2)))
        _ <- io2esf[Unit,IO](checkGetSHACLInstances(_UniversityTeacher, model, List(teacher2)))
        _ <- io2esf[Unit,IO](checkGetSHACLInstances(_Dog, model, List(dog1)))
        _ <- io2esf[Unit,IO](checkGetSHACLInstances(_Any, model, List()))
      } yield (())
      run_es(r).unsafeRunSync.fold(e => fail(s"Error: $e"), v => info("End getSHACL instances"))
    }

    def checkGetSHACLInstances(cls: RDFNode, model:Model, expected: List[RDFNode]): IO[Unit] = for {
      is <- JenaUtils.getSHACLInstances(cls,model)
    } yield it(s"instances of $cls should be ${expected.mkString(",")}") { 
      is should contain theSameElementsAs(expected) 
    }
  }

  describe("getValuesFromPath") {
    it("Validates parent") {
      val ex = "http://example.org#"
      val rdfStr =s"""|@prefix : <$ex> .
                    |:homer :parent :bart .
                    |:homer :parent :lisa .
                    |:homer :parent :maggie .
                    |:abraham :parent :homer .
                    |:abraham :parent :herb .
                    |""".stripMargin
      JenaUtils.parseFromString(rdfStr).attempt.unsafeRunSync match {
        case Right(model) => {
          val abraham = model.createResource(ex + "abraham")
          val homer = model.createResource(ex + "homer")
          val herb = model.createResource(ex + "herb")
          val bart = model.createResource(ex + "bart")
          val lisa =  model.createResource(ex + "lisa")
          val maggie = model.createResource(ex + "maggie")
          val parent = model.createResource(ex + "parent")
          val parent1 = new P_Link(parent.asNode)
          val parentPlus = new P_OneOrMoreN(parent1)
          JenaUtils.objectsFromPath(abraham, parent1, model) should contain only (herb, homer)
          JenaUtils.objectsFromPath(abraham, parentPlus, model) should contain only (bart, lisa, maggie, herb, homer
          )
        }
        case Left(msg) => fail(msg)
      }
    }
  }
}

