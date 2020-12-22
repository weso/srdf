package es.weso.rdf.locations
import es.weso.rdf.nodes._
import es.weso.rdf.triples._

case class Location(line: Long, col: Long, tokenType: String, source: Option[IRI] = None) {
  override def toString = s"${tokenType}@${line},${col}${source.map(x => s"<-${x.str}").getOrElse("")}"  
}

abstract class LocationsReader(
    nodesLocations: Map[RDFNode, Set[Location]], 
    triplesLocations: Map[RDFTriple, Set[Location]]
)