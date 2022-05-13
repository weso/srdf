package es.weso.rdf

import es.weso.rdf.nodes.IRI
import cats.implicits._

// See: https://www.w3.org/TR/sparql11-InferenceEngine/

abstract class InferenceEngine extends Product with Serializable {
  def name: String
}

case object NONE extends InferenceEngine {
  def name = "NONE"
}

case object RDFS extends InferenceEngine {
  def name = "RDFS"
}

case object OWL extends InferenceEngine {
  def name = "OWL"
}

object InferenceEngine {

  def availableInferenceEngines: List[InferenceEngine] = List(NONE, RDFS, OWL)
  def availableInferenceEngineNames: List[String] = availableInferenceEngines.map(_.name)
  def fromString(name: String): Either[String, InferenceEngine] =
    availableInferenceEngines
      .filter(_.name.compareToIgnoreCase(name) == 0)
      .headOption
      .toRight(s"""|Not found $name. 
                   |Available inference names: ${availableInferenceEngineNames.mkString(
                    ",")}""".stripMargin)
}
