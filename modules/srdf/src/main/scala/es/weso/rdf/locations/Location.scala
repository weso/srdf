package es.weso.rdf.locations
import es.weso.rdf.nodes._

case class Location(line: Long, col: Long, tokenType: String, source: Option[IRI] = None) {
  override def toString =
    s"${tokenType}@${line},${col}${source.map(x => s"<-${x.str}").getOrElse("")}"
}
