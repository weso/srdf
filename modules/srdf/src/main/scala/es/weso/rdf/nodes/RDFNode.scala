package es.weso.rdf.nodes

import cats.Show

abstract class RDFNode {
  def isIRI : Boolean
  def isBNode : Boolean
  def isLiteral : Boolean
  def isNonLiteral = this.isIRI || this.isBNode

  def toIRI: Either[String,IRI] = this match {
    case i: IRI => Right(i)
    case _ => Left(s"Cannot convert node $this to IRI")
  }

  def getLexicalForm: String

  def isEqualTo(other: RDFNode): Either[String,Boolean]

  def lessThan(other: RDFNode): Either[String,Boolean]

  // TODO: Could be optimized to short-circuit
  def lessThanOrEquals(other: RDFNode) = for {
    b1 <- isEqualTo(other)
    b2 <- lessThan(other)
  } yield b1 || b2

  def relativize(base: IRI): RDFNode = this match {
    case iri: IRI => iri.relativizeIRI(base)
    case other => other
  }

}

object RDFNode {
  val xsd = "http://www.w3.org/2001/XMLSchema#"
  val rdfSyntax = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val StringDatatypeIRI = IRI(xsd + "string")
  val RDFhtmlStringDatatypeIRI = IRI(rdfSyntax + "HTML") // RH20220819
  val LangStringDatatypeIRI = IRI(rdfSyntax + "langString")
  val BooleanDatatypeIRI = IRI(xsd + "boolean")
  val IntegerDatatypeIRI = IRI(xsd + "integer")
  val DoubleDatatypeIRI = IRI(xsd + "double")
  val DecimalDatatypeIRI = IRI(xsd + "decimal")
  val rdftype = IRI(rdfSyntax + "type")
  val rdfnil = IRI(rdfSyntax + "nil")
  val rdffirst = IRI(rdfSyntax + "first")
  val rdfrest = IRI(rdfSyntax + "rest")

  val trueLiteral = BooleanLiteral(true)
  val falseLiteral = BooleanLiteral(false)

  def qNameIRI(prefix: IRI, local: String): IRI = {
    IRI(prefix.str + local)
  }

  implicit val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    final def show(n: RDFNode): String = n.toString
  }

  /**
    * The order assumes that:
    *  IRIs > BNode > Literal
    * TODO: Review if there is already some convention about the ordering
    */
  implicit val orderingRDFNode: Ordering[RDFNode] = (x: RDFNode, y: RDFNode) => {
    val Bigger = 1
    val Lower = -1
    (x,y) match {
    case (iri1: IRI, iri2: IRI) => Ordering[IRI].compare(iri1,iri2)
    case (iri: IRI, _) => Bigger
    case (bnode: BNode, iri: IRI) => Lower
    case (bnode1: BNode, bnode2: BNode) => 
     Ordering[String].compare(bnode1.getLexicalForm,bnode2.getLexicalForm)
    case (bnode: BNode, lit: Literal) => Bigger
    case (lit: Literal, _) => Lower
    case (lit1: Literal, lit2: Literal) => 
     if (lit1.dataType == lit2.dataType)
      Ordering[String].compare(lit1.getLexicalForm, lit2.getLexicalForm) 
     else 
      Ordering[IRI].compare(lit1.dataType,lit2.dataType)
    }
  }
   

  def fromString(s: String): Either[String, RDFNode] = {
    val iriRegex = raw"<(.*)>".r
    val bNodeRegex = raw"_:(.*)".r
    val literalRegex = "\"(.*)\"".r
    val integerRegex = raw"(\d*)".r
    s match {
      case iriRegex(iri) => Right(IRI(iri))
      case bNodeRegex(bnodeId) => Right(BNode(bnodeId))
      case literalRegex(str) => Right(StringLiteral(str))
      case integerRegex(s) => {
        try Right(IntegerLiteral(s.toInt,s))
        catch {
          case e: NumberFormatException =>
            Left(s"Error parsing as integer: $e")
        }
      }
      case other => IRI.fromString(other,None).fold(
        e => Left(s"Error parsing String $other as RDFNode: $e"),
        iri => Right(iri))
    }
  }

}
