package es.weso.rdf.nodes

case class RDFhtmlStringLiteral(lexicalForm: String) extends Literal {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val rdfSyntax = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    val StringDatatypeIRI: IRI = IRI(xsd + "string")
    val RDFhtmlStringDatatypeIRI: IRI =  IRI(rdfSyntax + "HTML")

  // TODO: write a condition to see if we need rdf:HTML -- RH20220819

    val MyStringDatatypeIRI: IRI = if (false) StringDatatypeIRI else RDFhtmlStringDatatypeIRI // StringDatatypeIRI

  override val dataType : IRI = RDFhtmlStringDatatypeIRI // MyStringDatatypeIRI // StringDatatypeIRI

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    // TODO: Check if literal contains extended chars
    "\"" + lexicalForm + "\"^^rdf:HTML"
  }

  override def getLexicalForm = lexicalForm

  override def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case StringLiteral(s) => Right(s == lexicalForm)
    case _ => Right(false)
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case StringLiteral(str) => Right(lexicalForm < str)
    case _ => Left(s"Cannot compare string literal $this < $other which is not a string")
  }
}

