package es.weso.rdf.rdf4j

import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import org.scalatest._
import funspec.AnyFunSpec
import matchers.should.Matchers

class RDF4jMapperTest extends AnyFunSpec with Matchers with EitherValues with OptionValues {

  describe(s"rdfTriples2Model") {
    it(s"Should create model from triples") {
      val t1 = RDFTriple(IRI("http://example.org/x"), IRI("http://example.org/p"),StringLiteral("Hi"))
      val t2 = RDFTriple(IRI("http://example.org/x"), IRI("http://example.org/p"),BNode("x"))
      val t3 = RDFTriple(IRI("http://example.org/y"), IRI("http://example.org/p"),BNode("x"))
      val ts = Set(t1,t2,t3)
      val result = for {
        model <- RDF4jMapper.rdfTriples2Model(ts)
        rdf = RDFAsRDF4jModel(model)
        ts <- rdf.triplesWithSubject(IRI("http://example.org/x")).compile.toList
      } yield ts
      result.unsafeRunSync should contain theSameElementsAs(List(t1,t2)) 
    }
  }

}