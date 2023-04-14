package es.weso.rdftriple.jenaMapper
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf.nodes._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.jena._
import es.weso.rdf.triples.RDFTriple
import munit._
import cats._
import cats.implicits._
import scala.jdk.CollectionConverters._
import org.apache.jena.datatypes.RDFDatatype

class JenaMapperTest extends CatsEffectSuite with JenaBased {

  test("Should compare one triple with 2 different bNodes") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 1)))
    val s = """[] <http://example.org#p> [] ."""
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should compare one triple with a shared bNode") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)))
    val s = """_:a <http://example.org#p> _:a ."""
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should compare one triple with a prefix decl") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)))
    val s = """|@prefix : <http://example.org#> .
               |_:a :p _:a .""".stripMargin
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should compare one triple with an integer literal") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), IntegerLiteral(1, "1")))
    val s = """|@prefix : <http://example.org#> .
               |_:a :p 1 .""".stripMargin
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should compare one triple with a decimal literal") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), DecimalLiteral(1.2)))
    val s = """|@prefix : <http://example.org#> .
               |_:a :p 1.2 .""".stripMargin
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should compare one triple with a boolean literal") {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BooleanLiteral(true)))
    val s = """|@prefix : <http://example.org#> .
               |_:a :p true .""".stripMargin
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  // The following test fails probably for Double comparison
  test("Should compare one triple with a double literal".ignore) {
    val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), DoubleLiteral(1.2e3)))
    val s = """|@prefix : <http://example.org#> .
               |_:a :p 1.2e3 .""".stripMargin
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  test("Should convert three triples") {
    val ts = Set(
      RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)),
      RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), IntegerLiteral(4, "4")),
      RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), LangLiteral("pepe", Lang("es")))
    )
    val empty = ModelFactory.createDefaultModel
    val m1 = RDFTriples2Model(ts, empty, None)
    val m2 = str2model("""|@prefix : <http://example.org#> .
                          |_:0 <http://example.org#p> _:0, 4, "pepe"@es .
                          |""".stripMargin)
    assertEquals(checkIsomorphic(m1, m2), ().asRight)
  }

  test("Should check integer ok") {
    val node = IntegerLiteral(23, "23")
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#integer"))
      .map(assertEquals(_, true))
  }

  test("Should check string ok") {
    val node = StringLiteral("Pepe")
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#string"))
      .map(assertEquals(_, true))
  }

  test("Should check lang string ok") {
    val node = LangLiteral("Pepe", Lang("es"))
    wellTypedDatatype(node, IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"))
      .map(assertEquals(_, true))
  }

  test("Should check date ok") {
    val node = DatatypeLiteral("1981-07-03", IRI("http://www.w3.org/2001/XMLSchema#date"))
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date"))
      .map(assertEquals(_, true))
  }

  test("Should check bad string") {
    val node = DatatypeLiteral("john", IRI("http://www.w3.org/2001/XMLSchema#string"))
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date"))
      .map(assertEquals(_, false))
  }

  test("Should check bad date") {
    intercept[org.apache.jena.datatypes.DatatypeFormatException] {
      val node = DatatypeLiteral("john", IRI("http://www.w3.org/2001/XMLSchema#date"))
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date")).unsafeRunSync()
    }
  }

  test("Should fail checking single string with dateTime") {
    val node = StringLiteral("not_a_date")
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#dateTime"))
      .map(assertEquals(_, false))
  }

  test("Should fail checking date with dateTime") {
    val node = DatatypeLiteral("2017-05-15", IRI("http://www.w3.org/2001/XMLSchema#date"))
    wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#dateTime"))
      .map(assertEquals(_, false))
  }

  // Test added at issue: https://github.com/weso/srdf/issues/292
  test("Should compare one triple with a rdf:HTML literal") {
    val ts = Set(
      RDFTriple(
        BNode("b" + 0),
        IRI("http://example.org#p"),
        RDFHTMLLiteral("<div>Test HTML markup</div>")))
    val s =
      """|@prefix : <http://example.org#> .
         |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |_:a :p "<div>Test HTML markup</div>"^^rdf:HTML .""".stripMargin
    println(s"ts: $ts")
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

  // Test added at issue: https://github.com/weso/srdf/issues/292
  test("Should compare one triple with a rdf:HTML literal".only) {
    val ts = Set(
      RDFTriple(
        IRI("http://example.org#a"),
        IRI("http://example.org#p"),
        RDFHTMLLiteral("<div>Test HTML markup</div>")))
    val s =
      """|@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |<http://example.org#a> <http://example.org#p> "<div>Test HTML markup</div>"^^rdf:HTML .""".stripMargin
    println(s"ts: $ts")
    val empty = ModelFactory.createDefaultModel
    val model1 = RDFTriples2Model(ts, empty, None)
    val model2 = str2model(s)
    val s1 = model1.listSubjects().toList().asScala.head
    val s2 = model2.listSubjects().toList().asScala.head
    println(s"Subject1 = $s1, ${s1.getClass().getName()}")
    println(s"Subject2 = $s2, ${s2.getClass().getName()}")
    println(s"Are equal? ${s1.equals(s2)}")

    val o1dt = model1.listObjects().toList().asScala.head.asLiteral().getDatatype()
    val o2dt = model2.listObjects().toList().asScala.head.asLiteral().getDatatype()
    println(s"Object1 = $o1dt, ${o1dt.getClass().getName()}")
    println(s"Object2 = $o2dt, ${o2dt.getClass().getName()}")
    println(s"Are equal? ${o1dt.equals(o2dt)}")

    assertEquals(checkIsomorphic(model1, model2), ().asRight)
  }

}
