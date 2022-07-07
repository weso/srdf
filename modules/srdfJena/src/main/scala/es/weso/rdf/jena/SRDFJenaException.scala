package es.weso.rdf.jena

import java.io.File
import es.weso.rdf.RDFException

sealed abstract class SRDFJenaException protected (msg: String)
    extends RDFException(msg)
    with Product
    with Serializable

object SRDFJenaException {
  final case class UnsupportedFormat(format: String)
      extends SRDFJenaException(s"Unsupported format: $format")
  final case class UnsupportedInference(inference: String)
      extends SRDFJenaException(s"Unsupported inference: $inference")
  final case class FromUriException(uri: String, exc: Throwable)
      extends SRDFJenaException(s"Error obtaining RDF from URI $uri:${exc.getLocalizedMessage}")
  final case class FromFileException(file: File, exc: Throwable)
      extends SRDFJenaException(
        s"Error obtaining RDF from file ${file.toPath}: ${exc.getLocalizedMessage}")
  final case class FromStringException(str: String, exc: Throwable)
      extends SRDFJenaException(
        s"Error obtaining RDF from string\n$str\n: ${exc.getLocalizedMessage}")
}
