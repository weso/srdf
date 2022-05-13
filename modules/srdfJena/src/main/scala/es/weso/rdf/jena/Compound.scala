package es.weso.rdf.jena
// import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.nodes.{RDFNode, _}
import es.weso.rdf.path.SHACLPath
import es.weso.rdf.triples.RDFTriple
import io.circe.Json
import cats.effect._
import fs2.Stream
import org.slf4j._
import cats.implicits._
// import fs2.Stream
import es.weso.utils.IOUtils._

case class Compound(members: List[RDFReader]) extends RDFReader with RDFReasoner {

  type Rdf = Compound

  val id = s"Compound"

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: IO[PrefixMap] = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    IO(PrefixMap(Map()))
  }

  val log = LoggerFactory.getLogger("Endpoint")

  /*  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[IRI]): RDFRead[Compound] = {
    err("Cannot parse into a compound")
  } */

  override def serialize(format: String, base: Option[IRI]): RDFRead[String] = {
    err(s"Endpoint cannot be serialized to $format")
  }

  override def iris(): RDFStream[IRI] =
    mkStream(members, (e: RDFReader) => e.iris())

  override def subjects(): RDFStream[RDFNode] =
    mkStream(members, (e: RDFReader) => e.subjects())

  override def predicates(): RDFStream[IRI] =
    mkStream(members, (e: RDFReader) => e.predicates())

  override def iriObjects(): RDFStream[IRI] =
    mkStream(members, (e: RDFReader) => e.iriObjects())

  override def getSHACLInstances(c: RDFNode): RDFStream[RDFNode] =
    mkStream(members, (e: RDFReader) => e.getSHACLInstances(c))

  private def someTrue(xs: Set[Boolean]): Boolean = xs.exists(_ == true)

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = {
    val vs = members.map(_.hasSHACLClass(n, c))
    vs.sequence.map(_.toSet).map(someTrue(_))
  }

  override def nodesWithPath(path: SHACLPath): RDFStream[(RDFNode, RDFNode)] =
    mkStream(members, (e: RDFReader) => e.nodesWithPath(path))

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] =
    mkStream(members, (e: RDFReader) => e.subjectsWithPath(path, obj))

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] =
    mkStream(members, (e: RDFReader) => e.objectsWithPath(subj, path))

  override def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  override def rdfTriples(): RDFStream[RDFTriple] =
    mkStream(members, (e: RDFReader) => e.rdfTriples())

  def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] =
    mkStream(members, (e: RDFReader) => e.triplesWithSubject(node))

  def triplesWithPredicate(p: IRI): RDFStream[RDFTriple] = {
    mkStream(members, (e: RDFReader) => e.triplesWithPredicate(p))
  }

  def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = {
    mkStream(members, (e: RDFReader) => e.triplesWithObject(node))
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] = {
    mkStream(members, (e: RDFReader) => e.triplesWithPredicateObject(p, o))
  }

  override def applyInference(inference: InferenceEngine): RDFRead[Rdf] = {
    inference match {
      case NONE => ok(this)
      case other => err(s"Unsupported inference $other for compound model")
    }
  }

  override def availableInferenceEngines: List[InferenceEngine] = List(NONE)

  override def querySelect(queryStr: String): RDFStream[Map[String, RDFNode]] =
    Stream.raiseError[IO](new RuntimeException("Not implemented querySelect for Compound"))

  override def queryAsJson(queryStr: String): RDFRead[Json] = err(
    s"Nor implemented queryAsJson for compound")

  override def getNumberOfStatements(): RDFRead[Int] = err(
    s"Not implemented getNumberOfStatements of compound")

  override def isIsomorphicWith(other: RDFReader): RDFRead[Boolean] =
    err(s"Unimplemented isIsomorphicWith between endpoints")

  override def asRDFBuilder: RDFRead[RDFBuilder] =
    err(s"Unimplemented isIsomorphicWith between endpoints")

  override def rdfReaderName: String = s"Compound"

  override def sourceIRI: Option[IRI] = None

  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] = err(
    s"Not implemented hasPredicateWithSubject")

}
