package es.weso.rdf.jena

import java.io.ByteArrayOutputStream

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.internal.CollectionCompat
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import scala.util.{Either, Left, Right, Try}
import org.apache.jena.rdf.model.Property
import org.apache.jena.rdf.model.Statement
import org.apache.jena.rdf.model.Model
import org.slf4j._
import es.weso.rdf._
import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.jena.JenaMapper._
import es.weso.utils.EitherUtils
import es.weso.utils.IOUtils._
import cats.effect._

case class Endpoint(endpointIRI: IRI)
  extends RDFReader
     with RDFReasoner
     with LazyLogging {
  type Rdf = Endpoint

  val endpoint = endpointIRI.str
  val id = s"Endpoint($endpoint)"

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: PrefixMap = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    PrefixMap(Map())
  }

  val log = LoggerFactory.getLogger("Endpoint")

  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[IRI]): IO[Endpoint] = {
    err("Cannot parse into an endpoint. endpoint = " + endpoint)
  }

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

  override def predicates(): RDFStream[Set[IRI]] = Try {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findPredicates).execSelect()
    val s: Set[IRI] = resultSet.asScala.map(qs => IRI(qs.get("p").asResource.getURI)).toSet
    IO(s.toList)
  }.fold(e => errStream(s"predicates exception: ${e.getMessage}"), streamFromIOs(_))

  override def iriObjects(): RDFStream[Set[IRI]] = Try {
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
        EitherUtils.sequence(rs)
      } catch {
        case e: Exception => Left(s"getSHACLInstances: ${e.getMessage}")
      }
      case l: Literal => Right(Seq())
      case bn => Left(s"getSHACLInstances not implemented for blank node $c on endpoint ${endpoint}")
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
    val rs = resultSet.asScala.map(qs => get2Vars(qs,"x","y")).toList
    val r = EitherUtils.sequence(rs)
    r.map(_.toSet)
  }


  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] = obj match {
    case iri: IRI => Try {
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, querySubjectsWithPath(iri,path)).execSelect()
      val rs = resultSet.asScala.map(qs => getVar(qs,"x")).toList
      val r = EitherUtils.sequence(rs)
      r.map(_.toSet)
    }.fold(e => Left(s"objectsWithPath($obj,$path): exception: $e"), identity)
    case _ => Left(s"subjectsWithPath not implemented for non IRI nodes. Node: $obj, path: $path")
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] = subj match {
    case iri: IRI => Try {
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, queryObjectsWithPath(iri,path)).execSelect()
      val rs = resultSet.asScala.map(qs => getVar(qs,"x")).toList
      val r = EitherUtils.sequence(rs)
      r.map(_.toSet)
    }.fold(e => errStream(s"objectsWithPath($subj,$path): exception: $e"), identity)
    case _ => errStream(s"objectsWithPath not implemented for non IRI nodes. Node: $subj, path: $path")
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)


  override def rdfTriples(): RDFStream[RDFTriple] = Try {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriples).execConstruct()
    IO(model2triples(model).toList)
  }.fold(e => errStream(s"Exception obtaining rdfTriples of endpoint: $endpoint: $e"), streamFromIOs(_))

  override def triplesWithSubjectPredicate(node: RDFNode,
                                           p: IRI
                                          ): RDFStream[RDFTriple] = node match {
    case subj: IRI => Try {
      println(s"##<<< triplesWithSubjectPredicate($node, $p)")
      val query = queryTriplesWithSubjectPredicate(subj,p)
      println(s"Query: $query")
      val qExec = QueryExecutionFactory.sparqlService(endpoint,query)
      println(s"QueryExecutor: $qExec")
      val model = qExec.execConstruct
      // val it = qExec.execConstructTriples.forEachRemaining()
      println(s"###<<< triplesWithSubjectPredicate. End of query: ${model.size} triples retrieved")
      model2triples(model)
    }.fold(e => Left(s"Error accessing endpoint ${endpoint} to obtain triples with subject $node and predicate ${p}: ${e.getMessage}"),
      Right(_)
    )
    case _ => Right(Set()) // Left("triplesWithSubject: node " + node + " must be a IRI")
  }


  def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case subj: IRI => Try {
      println(s"## triplesWithSubject($node)")
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithSubject(subj)).execConstruct()
      println(s"### triplesWithSubjectPredicate. End of query: ${model.size} triples retrieved")
      model2triples(model)
    }.fold(e => Left(s"Error accessing endpoint ${endpoint} to obtain triples with subject $node: ${e.getMessage}"),
      Right(_)
    )
    case _ => Right(Set()) // Left("triplesWithSubject: node " + node + " must be a IRI")
  }

  def triplesWithPredicate(p: IRI): RDFStream[RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicate(p)).execConstruct()
    Right(model2triples(model))
  }

  def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case obj: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithObject(obj)).execConstruct()
      Right(model2triples(model))
    }
    case _ => Left("triplesWithObject: node " + node + " must be a IRI")
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] =
    o match {
    case iri: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicateObject(p, iri)).execConstruct()
      Right(model2triples(model))
    }
    case _ => Left("triplesWithPredicateObject: o " + o + " must be a IRI")
  }

  /* TODO: Remove the following code using JenaMapper methods */
  private def model2triples(model: Model): Set[RDFTriple] = {
    println(s"Model2triples: $model")
    val ts = model.listStatements().asScala.map(st => statement2triple(st)).toSet
    println(s"##<<< Total triples = ${ts.size}")
    ts
  }

  private def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      jena2rdfnode(st.getSubject),
      property2iri(st.getPredicate),
      jena2rdfnode(st.getObject))
  }

  private def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  private def jena2rdfnode(r: JenaRDFNode): RDFNode = {
    if (r.isAnon) {
      BNode(r.asNode.getBlankNodeId.getLabelString)
    } else if (r.isURIResource) {
      IRI(r.asResource().getURI())
    } else if (r.isLiteral) {
      val lit = r.asLiteral
      if (lit.getDatatypeURI() == null) {
        StringLiteral(lit.getString())
      } else
        IRI(lit.getDatatypeURI()) match {
          case RDFNode.IntegerDatatypeIRI => IntegerLiteral(lit.getInt)
          case RDFNode.BooleanDatatypeIRI => BooleanLiteral(lit.getBoolean)
          case RDFNode.DoubleDatatypeIRI => DoubleLiteral(lit.getDouble())
          case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
          case _ => DatatypeLiteral(lit.getLexicalForm, IRI(lit.getDatatypeURI))
        }
    } else
      throw new Exception("Unknown type of resource")
  }

  private def getVar(qs: QuerySolution, x: String): Either[String, RDFNode] = qs.get(x) match {
    case null => Left(s"Not found value for var $x in querySolution: $qs")
    case node => jenaNode2RDFNode(node)
  }

  private def get2Vars(qs: QuerySolution, x: String, y: String): Either[String, (RDFNode, RDFNode)] = for {
    v1 <- getVar(qs,x)
    v2 <- getVar(qs,y)
  } yield (v1,v2)

  override def applyInference(inference: String): Either[String, Rdf] = {
    inference.toUpperCase match {
      case "NONE" => Right(this)
      case other => Left(s"Unsupported inference $other for endpoint $endpoint")
    }
  }

  override def availableInferenceEngines: List[String] = List("NONE")

  override def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]] = {
    println(s"QuerySelect: $queryStr")
    val tryQuery: Try[List[Map[String,RDFNode]]] = Try {
      val query = QueryFactory.create(queryStr)
      val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result = qExec.execSelect()
          // val varNames = result.getResultVars
          val ls: List[Map[String, RDFNode]] = result.asScala.toList.map(qs => {
            val qsm = new QuerySolutionMap()
            qsm.addAll(qs)
            CollectionCompat.mapValues(qsm.asMap.asScala.view.toMap)(node => jenaNode2RDFNodeUnsafe(node))
//            qsm.asMap.asScala.view.mapValues(node => jenaNode2RDFNodeUnsafe(node)).toMap
          })
          ls
        }
        case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
      }
    }
    tryQuery.toEither.leftMap(_.getMessage)
  }

  override def queryAsJson(queryStr: String): Either[String, Json] = Try {
    println(s"QueryAsJson: $queryStr")
    val query = QueryFactory.create(queryStr)
    val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()

        val prologue = qExec.getQuery.getPrologue
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
  }.toEither.fold(f => Left(f.getMessage), es => es)

  override def getNumberOfStatements(): Either[String,Int] = {
    Try{
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, countStatements).execSelect()
      resultSet.asScala.map(qs => qs.get("c").asLiteral().getInt).toList.head
    }.toEither.leftMap(_.getMessage)
  }

  override def isIsomorphicWith(other: RDFReader): Either[String,Boolean] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def sourceIRI = None

  override def asRDFBuilder: Either[String,RDFBuilder] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def rdfReaderName: String = s"Endpoint($endpoint)"

}

object Endpoint {
  def fromString(str: String): IO[Endpoint] = {
    IRI.fromString(str).map(Endpoint(_))
  }
}