package es.weso.rdf.jena

// import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple

// import scala.collection.JavaConverters._
// import org.apache.jena.rdf.model.Property
// import org.apache.jena.rdf.model.Statement
// import org.apache.jena.rdf.model.Model
import org.slf4j._
// import org.apache.jena.riot.RDFDataMgr
// import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
// import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
// import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import cats.effect._
import fs2.Stream
// import es.weso.utils.internal.CollectionCompat.CollectionConverters
// import es.weso.utils.IOUtils._
// import JenaMapper._
import org.http4s.client._
import es.weso.utils.DerefUtils._
import es.weso.utils.IOUtils._
import scala.concurrent.ExecutionContext.global

/**
 * Obtains triples by redirect
 * @param prefixMap
 * @param maybeClient
 *   if specified, the requests will use http4s client, otherwise Java's httpClient
 */
case class RDFFromWeb(
    prefixMap: Option[PrefixMap] = None,
    maybeClient: Option[Client[IO]] = None
) extends RDFReader {

  type Rdf = RDFFromWeb

  val id = "RDFFromWeb"
  val log = LoggerFactory.getLogger("RDFFromWeb")

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: IO[PrefixMap] = {
    IO(prefixMap.getOrElse(PrefixMap.empty))
  }

  /*  override def fromString(cs: CharSequence, format: String, base: Option[IRI]): RDFRead[Rdf] = {
    err("Cannot parse RDFFromWeb ")
  } */

  override def serialize(format: String, base: Option[IRI]): RDFRead[String] = {
    err(s"Cannot serialize RDFFromWeb")
  }

  override def rdfTriples(): RDFStream[RDFTriple] = {
    errStream("Cannot obtain triples from RDFFromWeb ")
  }

  override def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] =
    errStream(s"""|triplesWithSubject: not implemented deref yet
                  |Node: $node 
                  |""".stripMargin)
  /*   node match {
     case subj: IRI => maybeClient match {
       case None => {
         for {
         rdf <- Stream.eval(derefRDFJava(subj.uri))
         ts <- rdf.triplesWithSubject(subj)
       } yield ts
       }
       case Some(client) =>
        errStream("triplesWithSubject: not implemented yet deref from client")
     }
    case _ => errStream("triplesWithSubject: node " + node + " must be a IRI")
  } */

  override def triplesWithPredicate(p: IRI): RDFStream[RDFTriple] =
    errStream(s"Cannot obtain triplesWithPredicate from dereferentiation")
  /*{
    val derefModel = ModelFactory.createDefaultModel
    RDFDataMgr.read(derefModel, p.str)
    val model = QueryExecutionFactory.create(queryTriplesWithPredicate(p), derefModel).execConstruct()
    streamFromIOs(model2triples(model))
  }*/

  override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] =
    errStream(s"Cannot obtain triples with Object by dereferentiation")
  /*   node match {
    case obj: IRI => {
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, obj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithObject(obj), derefModel).execConstruct()
      streamFromIOs(model2triples(model))
    }
    case _ =>
      errStream("triplesWithObject: node " + node + " must be a IRI")
  } */

  override def triplesWithPredicateObject(p: IRI, node: RDFNode): RDFStream[RDFTriple] =
    errStream(s"Cannot obtain triplesWithPredicateObject by dereferentiation")
  /*   node match {
     case obj: IRI => {
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, obj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithPredicateObject(p, obj), derefModel).execConstruct()
      streamFromIOs(model2triples(model))
    }
     case _ => errStream("triplesWithObject: node " + node + " must be a IRI")
  }  */

  override def getSHACLInstances(c: RDFNode): RDFStream[RDFNode] = {
    errStream(s"Undefined getSHACLInstances at RDFFromWeb. Node $c")
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = {
    err(s"hasSHACLClass: Not implemented at RDFFromWeb. Node: $n Class: $c")
  }

  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] =
    err(s"Not implemented hasPredicateWithSubject($n,$p)")

  override def nodesWithPath(p: SHACLPath): RDFStream[(RDFNode, RDFNode)] = {
    errStream(s"nodesWithPath: Undefined at RDFFromWeb. Path: $p")
  }

  override def subjectsWithPath(p: SHACLPath, o: RDFNode): RDFStream[RDFNode] = {
    errStream(s"Undefined subjectsWithPath at RDFFromWeb. Path: $p")
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] = {
    errStream(s"Undefined objectsWithPath at RDFFromWeb. Path: $path")
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  override def querySelect(queryStr: String): RDFStream[Map[String, RDFNode]] =
    Stream.raiseError[IO](new RuntimeException(s"Unimplemented query on RDFFromWeb"))

  override def queryAsJson(queryStr: String): RDFRead[Json] = err(
    s"Unimplemented query on RDFFromWeb")

  override def getNumberOfStatements(): RDFRead[Int] = err(
    s"Unimplemented number of statements of endpoint")

  override def isIsomorphicWith(other: RDFReader) = err(
    s"Unimplemented isomorphic test in RDFFromWeb")

  override def sourceIRI = None

  override def asRDFBuilder: RDFRead[RDFBuilder] =
    err(s"Cannot convert RDFFromWeb to RDFBuilder")

  override def rdfReaderName: String = s"RDFFromWeb"

}
