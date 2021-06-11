package es.weso.rdf.locations
import es.weso.rdf.nodes._
import es.weso.rdf.triples._
import io.circe.generic.auto._, io.circe.syntax._

case class Location(line: Long, col: Long, tokenType: String, source: Option[IRI] = None) {
  override def toString = s"${tokenType}@${line},${col}${source.map(x => s"<-${x.str}").getOrElse("")}"  
}

/* object Location {
  implicit val locationEncoder: Encoder[Location] = implicitly[Encoder[Location]]
} */
