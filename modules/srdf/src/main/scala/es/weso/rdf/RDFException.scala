package es.weso.rdf

import scala.util.control._

abstract class RDFException protected (val msg: String)
    extends Exception(msg)
    with NoStackTrace
    with Product
    with Serializable

case class MsgRDFException(message: String) extends RDFException(message)
case class ThrownException(message: String, e: Throwable) extends RDFException(message)

object RDFException {
  def fromString(msg: String): MsgRDFException = MsgRDFException(msg)
  def fromException(e: Throwable): ThrownException = ThrownException(e.getMessage, e)
}
