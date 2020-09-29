package es.weso.rdf.jena

import java.io.File

sealed abstract class SRDFJenaException extends Exception with Product with Serializable

object SRDFJenaException {
  final case class UnsupportedFormat(format: String) extends SRDFJenaException
  final case class UnsupportedInference(format: String) extends SRDFJenaException
  final case class FromUriException(uri: String, exc: Throwable) extends SRDFJenaException
  final case class FromFileException(file: File, exc: Throwable) extends SRDFJenaException
  final case class FromStringException(str: String, exc: Throwable) extends SRDFJenaException
}