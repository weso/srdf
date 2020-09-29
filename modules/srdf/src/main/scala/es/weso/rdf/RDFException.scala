package es.weso.rdf

sealed abstract class RDFException extends Exception

case class MsgRDFException(msg: String) extends RDFException {
    final override def getMessage: String = msg
}

case class ThrownException(msg: String, e: Throwable) extends RDFException {
    final override def getMessage: String = msg
}

object RDFException {
    def fromString(msg: String): MsgRDFException = MsgRDFException(msg)
    def fromException(e: Throwable): ThrownException = ThrownException(e.getMessage, e)
}