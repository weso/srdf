package es.weso.rdf.nodes
import cats.implicits._

case class RDFHTMLLiteral(lexicalForm: String) extends Literal {
  val rdfSyntax = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val RDFHTMLDatatypeIRI: IRI = IRI(rdfSyntax + "HTML")

  override val dataType: IRI = RDFHTMLDatatypeIRI

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    "\"" + lexicalForm + "\"^^rdf:HTML"
  }

  override def getLexicalForm = lexicalForm

  override def isEqualTo(other: RDFNode): Either[String, Boolean] = other match {
    case RDFHTMLLiteral(lf) => (lf == lexicalForm).asRight
    case _ => false.asRight
  }

  def lessThan(other: RDFNode): Either[String, Boolean] = other match {
    case RDFHTMLLiteral(lf) => (lexicalForm < lf).asRight
    case _ =>
      s"Cannot compare RDF HTML literal $this < $other which is not an RDF HTML literal".asLeft
  }
}
