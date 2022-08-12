package es.weso.rdf.nodes
import cats.Show

case class BNode(id: String) extends RDFNode {

  override def isLiteral: Boolean = false
  override def isBNode: Boolean = true
  override def isIRI: Boolean = false

  /* @deprecated */
  def newBNodeId: BNode = {
    val n = id.drop(1).toInt + 1
    BNode("b" + n)
  }

  override def toString: String = {
    Show[BNode].show(this)
  }

  override def getLexicalForm = id

  def isEqualTo(other: RDFNode): Either[String, Boolean] = other match {
    case b: BNode => Right(b.id == id)
    case _ => Left(s"Type error comaring $this with $other")
  }

  def lessThan(other: RDFNode): Either[String, Boolean] = other match {
    case b: BNode => Right(id < b.id)
    case _ => Left(s"Type error comaring $this with $other")
  }

}

object BNode {

  implicit val showBNode: Show[BNode] = new Show[BNode] {
    def show(b: BNode): String =
      "_:" + b.id
  }

}
