package es.weso.rdf.operations

import java.time.Instant

import cats._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
// import org.apache.xerces.impl.dv.{SchemaDVFactory, ValidatedInfo, XSSimpleType}
// import org.apache.xerces.impl.dv.xs._
// import org.apache.xerces.impl.validation.ValidationState
import cats.effect._
import scala.util._

// import scala.collection.compat._

object Comparisons {

  sealed trait PrimitiveLiteral

  case class Datetime(dt: Instant) extends PrimitiveLiteral

  sealed trait NumericLiteral {
    def totalDigits(): Int
    def fractionDigits(): Int
  }

  case class NumericInt(n: Int, repr: String) extends NumericLiteral {
    override def totalDigits() = repr.length
    override def fractionDigits() = 0
  }

  case class NumericDouble(n: Double, repr: String) extends NumericLiteral {
    override def totalDigits() = throw ErrorTotalDigits(repr)
    override def fractionDigits() = throw ErrorFractionDigits(repr)
  }

  case class NumericDecimal(n: BigDecimal, repr: String) extends NumericLiteral {
    override def totalDigits() = getTotalDigitsDecimal(repr).fold(e => throw e, identity)
    override def fractionDigits() = getFractionDigitsDecimal(repr).fold(e => throw e, identity)
  }

  private def str2NumericInt(str: String): Either[String, NumericInt] = try {
    val n: Int = Integer.parseInt(str)
    Right(NumericInt(n, str))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  private def str2NumericDecimal(str: String): Either[String, NumericDecimal] = try {
    val n: BigDecimal = BigDecimal(str)
    Right(NumericDecimal(n,str))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  private def str2NumericDouble(str: String): Either[String, NumericDouble] = try {
    val n: Double = str.toDouble
    Right(NumericDouble(n,str))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  def numericValue(node: RDFNode): Either[String, NumericLiteral] = node match {
    case IntegerLiteral(i, repr) => NumericInt(i,repr).asRight
    case DoubleLiteral(d, repr) => NumericDouble(d,repr).asRight
    case DecimalLiteral(d, repr) => NumericDecimal(d,repr).asRight
    case DatatypeLiteral(str, `xsd:byte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:decimal`) => str2NumericDecimal(str)
    case DatatypeLiteral(str, `xsd:double`) => str2NumericDouble(str)
    case DatatypeLiteral(str, `xsd:int`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:integer`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:long`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:positiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:negativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:nonPositiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:nonNegativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:short`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedLong`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedInt`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedShort`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedByte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:float`) => str2NumericDouble(str)
    case DatatypeLiteral(str, other) => Left(s"Cannot convert to numeric value datatype literal $str^^$other")
    case _ => Left(s"Cannot convert $node to numeric literal for comparison")
  }

  def lessThanOrEquals(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericInt(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericInt(n1,_), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 <= n2
  }

  def lessThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericInt(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericInt(n1,_), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 < n2
  }

  def greaterThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = lessThan(nl2,nl1)
  def greaterThanOrEquals(nl1:NumericLiteral,nl2: NumericLiteral): Boolean = lessThanOrEquals(nl2,nl1)


  def lessThanOrEquals(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = node1.lessThanOrEquals(node2)
  def lessThan(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = node1.lessThan(node2)
  def greaterThanOrEquals(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = node2.lessThanOrEquals(node1)
  def greaterThan(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = node2.lessThan(node1)

  type E[A] = Either[String,A]

  
  def contains[F[_]: Foldable](ns: F[RDFNode], node: RDFNode): Either[String,Boolean] = {
    existsM(ns, n => node.isEqualTo(n))
  }

  def existsM[F[_]: Foldable, A](ns: F[A], p: A => Either[String, Boolean]): Either[String,Boolean] = 
    Foldable[F].existsM[E,A](ns)(n => p(n))

  def notContained(ns: List[RDFNode], targets: List[RDFNode]): Either[String,List[RDFNode]] = {
    val zero: List[RDFNode] = List()
    def cmb(rest: List[RDFNode], a: RDFNode): Either[String,List[RDFNode]] =
      contains(targets,a).map(b => if (b) rest else (a :: rest))
    // Foldable[List].foldM(ns,zero)(cmb)
    Foldable[List].foldM[E,RDFNode,List[RDFNode]](ns,zero)(cmb)
  }

  def different(ns1: List[RDFNode], ns2: List[RDFNode]): Either[String,List[RDFNode]] = for {
    d1 <- notContained(ns1,ns2)
    d2 <- notContained(ns2,ns1)
  } yield List.concat(d1, d2)

   // TODO Remove dependency on Xerces
  /* def getTotalDigitsDecimal(value: String): Either[ErrorTotalDigits, Int] = Try {
    val context = new ValidationState
    val decimalDV = new DecimalDV()
    val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
    val resultInfo = new ValidatedInfo
    typeDeclaration.validate(value, context, resultInfo)
    decimalDV.getTotalDigits(resultInfo.actualValue)
  }.fold(
    e => ErrorTotalDigits(value, e.some).asLeft,
    n => n.asRight
  ) */

  // Based on this: https://stackoverflow.com/questions/58189457/how-to-count-digits-in-bigdecimal
  def getTotalDigitsDecimal(value: String): Either[ErrorTotalDigits, Int] = Try {
    val bd = new java.math.BigDecimal(value)
    val n = bd.stripTrailingZeros()
    n.precision

  }.fold(
    e => ErrorTotalDigits(value, e.some).asLeft,
    _.asRight
  )
  

  case class ErrorTotalDigits(value: String, e: Option[Throwable] = None) 
   extends RuntimeException(s"Error obtaining total digits of $value ${e.fold("")(_.getLocalizedMessage())}")

  // TODO replace this by a builtin implementation
  /* This implementation leverages Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  /* def getFractionDigitsDecimal(value: String): Either[ErrorFractionDigits, Int] =
    Try {
      val context = new ValidationState
      val decimalDV = new DecimalDV()
      val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
      val resultInfo = new ValidatedInfo
      typeDeclaration.validate(value, context, resultInfo)
      decimalDV.getFractionDigits(resultInfo.actualValue)
    }.fold(
      e => ErrorFractionDigits(value, e.some).asLeft,
      n => n.asRight
    ) */

  def getFractionDigitsDecimal(value: String): Either[ErrorFractionDigits, Int] = Try {
    val bd = new java.math.BigDecimal(value)
    val n = bd.stripTrailingZeros()
    n.scale()
  }.fold(
    e => ErrorFractionDigits(value, e.some).asLeft,
    _.asRight
  )

  case class ErrorFractionDigits(value: String, e: Option[Throwable] = None) 
   extends RuntimeException(s"Error obtaining fraction digits of $value: ${e.fold("")(_.getLocalizedMessage())}")

}
