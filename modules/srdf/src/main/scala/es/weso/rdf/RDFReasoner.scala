package es.weso.rdf
import cats.effect._

trait RDFReasoner extends RDFReader {

  type Rdf <: RDFReasoner

  def applyInference(inference: InferenceEngine): IO[RDFReasoner]

  def availableInferenceEngines: List[InferenceEngine]

}

