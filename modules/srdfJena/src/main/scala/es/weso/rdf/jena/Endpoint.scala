package es.weso.rdf.jena

import java.io.ByteArrayOutputStream

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple
//import es.weso.utils.internal.CollectionCompat
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import scala.util._
//import org.apache.jena.rdf.model.Property
//import org.apache.jena.rdf.model.Statement
//import org.apache.jena.rdf.model.Model
import org.slf4j._
import es.weso.rdf._
import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import cats.implicits._
import es.weso.rdf.jena.JenaMapper._
import es.weso.utils.IOUtils._
import cats.effect._
import fs2.Stream
import es.weso.utils.StreamUtils._

case class Endpoint(endpointIRI: IRI)
  extends RDFReader
     with RDFReasoner {
  type Rdf = Endpoint

  val endpoint = endpointIRI.str
  val id = s"Endpoint($endpoint)"

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: IO[PrefixMap] = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    IO(PrefixMap(Map()))
  }

  val log = LoggerFactory.getLogger("Endpoint")

/*  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[IRI]): IO[Endpoint] = {
    err("Cannot parse into an endpoint. endpoint = " + endpoint)
  } */

  override def serialize(format: String, base: Option[IRI]): IO[String] = {
    err(s"Endpoint with url $endpoint. Cannot be serialized to $format")
  }

  override def iris(): RDFStream[IRI] = Try {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    val s: Set[IRI] = resultSet.asScala.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
    IO(s.toList)
  }.fold(e => errStream(s"iris exception: ${e.getMessage}"), streamFromIOs(_))

  override def subjects(): RDFStream[RDFNode] = Try {
    // TODO: The following code only returns resource IRIs (no BNodes)
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    val s: Set[RDFNode] = resultSet.asScala.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
    IO(s.toList)
  }.fold(e => errStream(s"subjects exception: ${e.getMessage}"), streamFromIOs(_))

  override def predicates(): RDFStream[IRI] = Try {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findPredicates).execSelect()
    val s: Set[IRI] = resultSet.asScala.map(qs => IRI(qs.get("p").asResource.getURI)).toSet
    IO(s.toList)
  }.fold(e => errStream(s"predicates exception: ${e.getMessage}"), streamFromIOs(_))

  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] = err(s"Endpoint: Not implemented hasPredicateWithSubject")

  override def iriObjects(): RDFStream[IRI] = Try {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    val s: Set[IRI] = resultSet.asScala.map(qs => IRI(qs.get("y").asResource.getURI)).toSet
    IO(s.toList)
  }.fold(e => errStream(s"iriObjects exception: ${e.getMessage}"), streamFromIOs(_))

  override def getSHACLInstances(c: RDFNode): RDFStream[RDFNode] = {
    c match {
      case iri: IRI => try {
        val resultSet = QueryExecutionFactory.sparqlService(endpoint, queryShaclInstances(iri)).execSelect()
        val rs = resultSet.asScala.map(qs => qs.get("x") match {
          case null => s"Not found value for variable in querySolution: $qs".asLeft[RDFNode]
          case r => {
            val node: RDFNode = IRI(r.asResource.getURI)
            node.asRight
          }
        }).toList
        rs.sequence.fold(errStream(_), Stream.emits(_))
      } catch {
        case e: Exception => errStream(s"getSHACLInstances: ${e.getMessage}")
      }
      case l: Literal => Stream.empty
      case bn => errStream(s"getSHACLInstances not implemented for blank node $c on endpoint ${endpoint}")
    }
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = (n,c) match {
    case (iriN: IRI, iriC: IRI) => {
      val b = QueryExecutionFactory.sparqlService(endpoint, queryHasShaclClass(iriN,iriC)).execAsk()
      ok(b)
   }
    case _ => ok(false)
  }

  override def nodesWithPath(path: SHACLPath): RDFStream[(RDFNode, RDFNode)] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, queryPath(path)).execSelect()
    val rs = resultSet.asScala.map(qs => get2Vars(qs,"x","y")).toList.sequence
    streamFromIOs(rs)
  }


  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] = obj match {
    case iri: IRI => Try {
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, querySubjectsWithPath(iri,path)).execSelect
      val rs = resultSet.asScala.map(qs => getVar(qs,"x")).toList.sequence
      streamFromIOs(rs)
    }.fold(e => errStream(s"objectsWithPath($obj,$path): exception: $e"), identity)
    case _ => errStream(s"subjectsWithPath not implemented for non IRI nodes. Node: $obj, path: $path")
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] = subj match {
    case iri: IRI => Try {
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, queryObjectsWithPath(iri,path)).execSelect()
      val rs = resultSet.asScala.map(qs => getVar(qs,"x")).toList.sequence
      streamFromIOs(rs)
    }.fold(e => errStream(s"objectsWithPath($subj,$path): exception: $e"), identity)
    case _ => errStream(s"objectsWithPath not implemented for non IRI nodes. Node: $subj, path: $path")
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)


  override def rdfTriples(): RDFStream[RDFTriple] = for {
    ts <- Try {
      val query = queryTriples()
      val model = QueryExecutionFactory.sparqlService(endpoint, query).execConstruct()
      model2triples(model)
    }.fold(
      e => errStream(s"Exception obtaining rdfTriples of endpoint: $endpoint: $e"),
      streamFromIOs(_))
  } yield ts

  override def triplesWithSubjectPredicate(node: RDFNode,
                                           p: IRI
                                          ): RDFStream[RDFTriple] = node match {
    case subj: IRI => Try {
      val query = queryTriplesWithSubjectPredicate(subj,p)
      val qExec = QueryExecutionFactory.sparqlService(endpoint,query)
      val model = qExec.execConstruct
      model2triples(model)
    }.fold(e => errStream(s"Error accessing endpoint ${endpoint} to obtain triples with subject $node and predicate ${p}: ${e.getMessage}"),
      streamFromIOs(_)
    )
    case _ => Stream.empty
  }


  def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case subj: IRI =>
      for {
        ts <- Try {
          val query = queryTriplesWithSubject(subj)
          val model = QueryExecutionFactory.sparqlService(endpoint, query).execConstruct()
          model2triples(model)
        }.fold(e => errStream(s"Error accessing endpoint ${endpoint} to obtain triples with subject $node: ${e.getMessage}"),
          streamFromIOs(_)
        )
      } yield ts
    case _ => Stream.empty
  }

  def triplesWithPredicate(p: IRI): RDFStream[RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicate(p)).execConstruct()
    streamFromIOs(model2triples(model))
  }

  override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case obj: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithObject(obj)).execConstruct()
      streamFromIOs(model2triples(model))
    }
    case _ => errStream("triplesWithObject: node " + node + " must be a IRI")
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] =
    o match {
    case iri: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicateObject(p, iri)).execConstruct()
      streamFromIOs(model2triples(model))
    }
    case _ => errStream("triplesWithPredicateObject: o " + o + " must be a IRI")
  }

  private def getVar(qs: QuerySolution, x: String): IO[RDFNode] = qs.get(x) match {
    case null => err(s"Not found value for var $x in querySolution: $qs")
    case node => jenaNode2RDFNode(node)
  }

  private def get2Vars(qs: QuerySolution, x: String, y: String): IO[(RDFNode, RDFNode)] = for {
    v1 <- getVar(qs,x)
    v2 <- getVar(qs,y)
  } yield (v1,v2)

  override def applyInference(inference: InferenceEngine): RDFRead[Rdf] = {
    inference match {
      case NONE => ok(this)
      case other => err(s"Unsupported inference $other for endpoint $endpoint")
    }
  }

  override def availableInferenceEngines: List[InferenceEngine] = List(NONE)

  override def querySelect(queryStr: String): RDFStream[Map[String,RDFNode]] = {
    Try {
      val query = QueryFactory.create(queryStr)
      val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result = qExec.execSelect()
          val ls: List[IO[Map[String, RDFNode]]] = 
           result.asScala.toList.map(qs => {
            val qsm = new QuerySolutionMap()
            qsm.addAll(qs)
            val pairs: List[(String, JenaRDFNode)] =
             qsm.asMap.asScala.view.toMap.toList
            val iom: IO[Map[String, RDFNode]] = 
             pairs.map { 
               case (v, jenaNode) => jenaNode2RDFNode(jenaNode).flatMap(node => ok((v, node))) 
              }.sequence.map(_.toMap)
            iom 
          })
          ls.sequence
        }
        case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
      }
    }.fold(Stream.raiseError[IO], fromIOLs)
  }

  override def queryAsJson(queryStr: String): IO[Json] = 
   Try {
    val query = QueryFactory.create(queryStr)
    val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()

        // val prologue = qExec.getQuery.getPrologue
        // val prefixMap: Map[String,String] = prologue.getPrefixMapping.getNsPrefixMap.asScala.toMap

        // TODO: Add prefixes and base to JSON result
//        val prefixes = PrefixMap(prefixMap.map { case (k,v) => (Prefix(k), IRI(v)) })
//        val base = prologue.getBaseURI()
        val outputStream = new ByteArrayOutputStream()
        ResultSetFormatter.outputAsJSON(outputStream, result)
        val jsonStr = new String(outputStream.toByteArray())
        /* val result = parse(jsonStr).leftMap(f => f.getMessage)
        val json = Json.fromFields(
          List(
            ("base", prologue.getBaseURI),
            ("prefixes", jsonPrefixes), 
            ("result", result) 
        ))
        json */
        parse(jsonStr).leftMap(f => f.getMessage)
      }
      case Query.QueryTypeConstruct => {
        // val result = qExec.execConstruct()
        Left(s"Unimplemented CONSTRUCT queries yet")
      }
      case Query.QueryTypeAsk => {
        val result = qExec.execAsk()
        Right(Json.fromBoolean(result))
      }
      case Query.QueryTypeDescribe => {
        Left(s"Unimplemented DESCRIBE queries yet")
      }
      case _ => {
        Left(s"Unknown type of query. Not implemented")
      }
    }
  }.fold(f => err(f.getMessage), _.fold(err(_), ok(_)))

  override def getNumberOfStatements(): IO[Int] = {
    Try{
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, countStatements).execSelect()
      resultSet.asScala.map(qs => qs.get("c").asLiteral().getInt).toList.head
    }.fold(IO.raiseError(_), ok(_))
  }

  override def isIsomorphicWith(other: RDFReader): IO[Boolean] =
    err(s"Unimplemented isIsomorphicWith between endpoints")

  override def sourceIRI = None

  override def asRDFBuilder: IO[RDFBuilder] =
    err(s"Unimplemented isIsomorphicWith between endpoints")

  override def rdfReaderName: String = s"Endpoint($endpoint)"

}

object Endpoint {
  def fromString(str: String): IO[Endpoint] = {
    fromES(IRI.fromString(str).map(Endpoint(_)))
  }
}