package es.weso.rdf
import cats.effect._
import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import PREFIXES._

trait RDFBuilder extends RDFReader {

  type Rdf <: RDFBuilder
  type RDFBuild[A] = IO[A]

  def addBase(iri: IRI): RDFBuild[Rdf]
  
  def addPrefixMap(pm: PrefixMap): RDFBuild[Rdf]

  def addPrefix(alias: String, iri: IRI): RDFBuild[Rdf]

  def createBNode: RDFBuild[(RDFNode, Rdf)]

//  def mkBNode: RDFBuild[(RDFNode, Rdf)] = createBNode

  def addTriples(triples: Set[RDFTriple]): RDFBuild[Rdf]

  def addTriple(triple: RDFTriple): RDFBuild[Rdf] = {
    addTriples(Set(triple))
  }

  def addType(node: RDFNode, typeNode: RDFNode): RDFBuild[Rdf] = {
    addTriple(RDFTriple(node, `rdf:type`, typeNode))
  }

  def rmTriple(triple: RDFTriple): RDFBuild[Rdf]

  def empty: Resource[RDFBuild,Rdf]

  def merge(other: RDFReader): RDFBuild[Rdf]

  def extendImports: RDFBuild[Rdf]

  def normalizeBNodes: RDFBuild[RDFBuilder]

  def fromString(str: String, format: String, base: Option[IRI]): Resource[IO, RDFBuilder]

}

