package es.weso.rdf
import cats.effect._

trait RDFReasoner extends RDFReader {

  type Rdf <: RDFReasoner

  def applyInference(inference: String): IO[RDFReasoner]

  def availableInferenceEngines: List[String]

}

