package es.weso.rdf.operations

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import munit._

import cats.implicits._

class ComparisonsTest extends FunSuite {

  shouldBeLessThan(DoubleLiteral(2.3), DoubleLiteral(2.4), true)
  shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(2,"2"), false)
  shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(3,"3"), true)
  shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.3,"2.3"), false)
  shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.34,"2.34"), true)

  def shouldBeLessThan(node1: RDFNode, node2: RDFNode, expected: Boolean): Unit = {
      test(s"lessThan(${node1.show}, ${node2.show}) should be $expected") {
        val r = node1 lessThan node2
        r.fold(e => fail(s"Error: $e"),
          v => assertEquals(v, expected)
        )
      }
  }

  
  shouldBeLessThanOrEquals(DoubleLiteral(2.3), DoubleLiteral(2.4), true)
  shouldBeLessThanOrEquals(DoubleLiteral(2.3), IntegerLiteral(2,"2"), false)
  shouldBeLessThanOrEquals(DoubleLiteral(2.3), IntegerLiteral(3,"3"), true)
  shouldBeLessThanOrEquals(DoubleLiteral(2.3), DecimalLiteral(2.3,"2.3"), true)
  shouldBeLessThanOrEquals(DoubleLiteral(2.3), DecimalLiteral(2.34,"2.34"), true)

  def shouldBeLessThanOrEquals(node1: RDFNode, node2: RDFNode, expected: Boolean): Unit = {
      test(s"lessThan(${node1.show}, ${node2.show}) should be $expected") {
        val r = node1 lessThanOrEquals node2
        r.fold(e => fail(s"Error: $e"),
          v => assertEquals(v, expected)
        )
      }
    }

  
    shouldContain(List(DoubleLiteral(2.0),IntegerLiteral(2,"2")), IntegerLiteral(2,"2"), true)
    shouldContain(List(DoubleLiteral(2.0),IntegerLiteral(2,"2")), IntegerLiteral(3,"3"), false)

    def shouldContain(ns: List[RDFNode], node: RDFNode, expected: Boolean): Unit = {
      test(s"contain(${ns.show}, ${node.show}) should be $expected") {
        val r = Comparisons.contains(ns, node)
        r.fold(e => fail(s"Error: $e"),
          v => assertEquals(v, expected)
        )
      }
    }

 
    shouldCheckNotContained(List(DoubleLiteral(3.0),IntegerLiteral(2,"2")), List(IntegerLiteral(2,"2")), List(DoubleLiteral(3.0)))
    shouldCheckNotContained(List(DoubleLiteral(2.0),IntegerLiteral(2,"2")), List(IntegerLiteral(2,"2")), List())
    shouldCheckNotContained(List(IntegerLiteral(2)), List(DoubleLiteral(3.0),IntegerLiteral(2)), List())
    shouldCheckNotContained(List(IntegerLiteral(2)), List(IntegerLiteral(3)), List(IntegerLiteral(2)))
    shouldCheckNotContained(List(IntegerLiteral(2)), List(), List(IntegerLiteral(2)))
    shouldCheckNotContained(List(), List(StringLiteral("hi")), List())

    def shouldCheckNotContained(ns: List[RDFNode], ts: List[RDFNode], expected: List[RDFNode]): Unit = {
      test(s"notContained(${ns.show}, ${ts.show}) should be ${expected.show}") {
        val r = Comparisons.notContained(ns, ts)
        r.fold(e => fail(s"Error: $e"),
          vs => assertEquals(vs, expected)
        )
      }
    }


    shouldCheckDifferent(List(DoubleLiteral(3.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List(DoubleLiteral(3.0)))
    shouldCheckDifferent(List(DoubleLiteral(2.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List())
    shouldCheckDifferent(List(IntegerLiteral(2)), List(DoubleLiteral(3.0),IntegerLiteral(2)), List(DoubleLiteral(3.0)))
    shouldCheckDifferent(List(IntegerLiteral(2)), List(IntegerLiteral(3)), List(IntegerLiteral(2), IntegerLiteral(3)))
    shouldCheckDifferent(List(IntegerLiteral(2)), List(), List(IntegerLiteral(2)))
    shouldCheckDifferent(List(), List(StringLiteral("hi")), List(StringLiteral("hi")))

    def shouldCheckDifferent(ns: List[RDFNode], ts: List[RDFNode], expected: List[RDFNode]): Unit = {
      test(s"different(${ns.show}, ${ts.show}) should be ${expected.show}") {
        val r = Comparisons.different(ns, ts)
        r.fold(e => fail(s"Error: $e"),
          vs => assertEquals(vs, expected)
        )
      }
    }

    shouldGetTotalDigits(IntegerLiteral(23,"23"), 2)
    shouldGetTotalDigits(DatatypeLiteral("23.45",`xsd:decimal`), 4)

    def shouldGetTotalDigits(n: RDFNode, expected: Int): Unit = {
      test(s"totalDigits(${n.show}) should be ${expected.show}") {
        val v = Comparisons.numericValue(n).map(_.totalDigits)
        v.fold(e => fail(s"Error: $e"),
          t => assertEquals(t, expected)
        )
      }
    }    

   shouldGetFractionDigits(IntegerLiteral(23,"23"), 0)
   shouldGetFractionDigits(DatatypeLiteral("23.45",`xsd:decimal`), 2)
   shouldGetFractionDigits(DatatypeLiteral("23.4567",`xsd:decimal`), 4)
   shouldGetFractionDigits(DatatypeLiteral("55.0",`xsd:decimal`), 0)
   shouldFailFractionDigits(DoubleLiteral(5.5E03,"5.123E0"))

   def shouldGetFractionDigits(n: RDFNode, expected: Int): Unit = {
      test(s"fractionDigits(${n.show}) should be ${expected.show}") {
        val v = Comparisons.numericValue(n).map(_.fractionDigits)
        v.fold(e => fail(s"Error: $e"),
          t => assertEquals(t, expected)
        )
      }
    }    

  def shouldFailFractionDigits(n: RDFNode): Unit = {
      test(s"fractionDigits(${n.show}) should fail") {
        intercept[Comparisons.ErrorFractionDigits](Comparisons.numericValue(n).map(_.fractionDigits))
      }
    }    


}