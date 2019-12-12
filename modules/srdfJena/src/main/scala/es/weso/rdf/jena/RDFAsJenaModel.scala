package es.weso.rdf.jena
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import scala.util.Try
import es.weso.rdf._
import org.apache.jena.rdf.model.{Model, Property, Resource, Statement, RDFNode => JenaRDFNode}
import org.slf4j._
import org.apache.jena.riot._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.lang._
import scala.util._
import java.io._
import org.apache.jena.riot.RDFLanguages._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.path.SHACLPath
import es.weso.utils._
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.query.{Query, QueryExecutionFactory, QuerySolutionMap, ResultSetFormatter}
import cats.implicits._
import org.apache.jena.graph.Graph
import org.apache.jena.riot.system.{StreamRDF, StreamRDFLib}
import org.apache.jena.sparql.util.Context
import cats.effect._
import es.weso.utils.IOUtils._
import cats.implicits._
import fs2.Stream

case class RDFAsJenaModel(model: Model, base: Option[IRI] = None, sourceIRI: Option[IRI] = None)
    extends RDFReader
    with RDFBuilder
    with RDFReasoner {

  val id = s"RDFAsJenaModel($sourceIRI)"

  type Rdf = RDFAsJenaModel

  val log = LoggerFactory.getLogger("RDFAsJenaModel")

  def availableParseFormats: List[String]     = RDFAsJenaModel.availableFormats
  def availableSerializeFormats: List[String] = RDFAsJenaModel.availableFormats

  override def fromString(cs: CharSequence, format: String, base: Option[IRI] = None): RDFRead[Rdf] =
    Try {
      val m               = ModelFactory.createDefaultModel
      val str_reader      = new StringReader(cs.toString)
      val baseURI         = base.getOrElse(IRI(""))
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      val ctx: Context    = null
      RDFParser.create
        .source(str_reader)
        .base(baseURI.str)
        .labelToNode(LabelToNode.createUseLabelEncoded())
        .lang(shortnameToLang(format))
        .context(ctx)
        .parse(dest)
      RDFAsJenaModel(m, base)
    }.fold(e => err(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs"), ok(_))

  /*  def fromStringIO(cs: CharSequence, format: String, base: Option[IRI] = None): IO[Either[String, Rdf]] =
    IO {
      Try {
        val m               = ModelFactory.createDefaultModel
        val str_reader      = new StringReader(cs.toString)
        val baseURI         = base.getOrElse(IRI(""))
        val g: Graph        = m.getGraph
        val dest: StreamRDF = StreamRDFLib.graph(g)
        val ctx: Context    = null
        RDFParser.create
          .source(str_reader)
          .base(baseURI.str)
          .labelToNode(LabelToNode.createUseLabelEncoded())
          .lang(shortnameToLang(format))
          .context(ctx)
          .parse(dest)
        RDFAsJenaModel(m, base)
      }.fold(e => Left(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs"), Right(_))
    } */

  private def getRDFFormat(formatName: String): Either[String, String] = {
    val supportedFormats: List[String] =
      RDFLanguages.getRegisteredLanguages().asScala.toList.map(_.getName.toUpperCase).distinct
    formatName.toUpperCase match {
      case format if supportedFormats.contains(format) => Right(format)
      case unknown                                     => Left(s"Unsupported format $unknown. Available formats: ${supportedFormats.mkString(",")} ")
    }
  }

  override def serialize(formatName: String, base: Option[IRI]): RDFRead[String] =
    Try {
      for {
        format <- getRDFFormat(formatName)
        str = {
          val out              = new ByteArrayOutputStream()
          val relativizedModel = JenaUtils.relativizeModel(model, base.map(_.uri))
          relativizedModel.write(out, format)
          out.toString
        }
      } yield str
    }.fold(
      e => err(s"Error serializing RDF to format $formatName: $e"),
      _.fold(e => err(s"Format $formatName not found: $e"), ok(_))
    )

  // TODO: this implementation only returns subjects
  override def iris(): Either[String, Set[IRI]] = {
    val resources: Set[Resource] = model.listSubjects().asScala.toSet
    Right(resources.filter(s => s.isURIResource).map(r => IRI(r.getURI)))
  }

  override def subjects(): RDFStream[Set[RDFNode]] = {
    val resources: Set[Resource] = model.listSubjects().asScala.toSet
    Stream.eval(sequence(resources.map(r => jenaNode2RDFNode(r)).toList).map(_.toSet))
  }

  override def rdfTriples(): RDFStream[RDFTriple] = {
    streamFromIOs(model2triples(model))
  }

  override def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case _: Literal => Stream.empty
    case _ =>
      streamFromIOs(for {
        resource   <- rdfNode2Resource(node, model, base)
        statements <- triplesSubject(resource, model)
        ts         <- toRDFTriples(statements)
      } yield ts)
  }

  override def triplesWithSubjectPredicate(node: RDFNode, p: IRI): RDFStream[RDFTriple] =
    streamFromIOs(for {
      r  <- rdfNode2Resource(node, model, base)
      ss <- triplesSubjectPredicate(r, p, model, base)
      ts <- toRDFTriples(ss)
    } yield ts)

  /**
    * return the SHACL instances of a node `cls`
    * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
    */
  override def getSHACLInstances(c: RDFNode): RDFRead[Seq[RDFNode]] =
    for {
      is <- JenaUtils.getSHACLInstances(JenaMapper.rdfNode2JenaNode(c, model, base), model)
      ns <- is.toList.map(n => JenaMapper.jenaNode2RDFNode(n)).sequence
    } yield ns

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = {
    val nJena = JenaMapper.rdfNode2JenaNode(n, model, base)
    val cJena = JenaMapper.rdfNode2JenaNode(c, model, base)
    JenaUtils.hasClass(nJena, cJena, model)
  }

  override def nodesWithPath(path: SHACLPath): RDFStream[(RDFNode, RDFNode)] =
    streamFromIOs(
      for {
        jenaPath <- JenaMapper.path2JenaPath(path, model, base)
        nodes    <- JenaUtils.getNodesFromPath(jenaPath, model)
        pairs <- {
          val v: List[IO[(RDFNode, RDFNode)]] = nodes.map(pair => {
            val (subj, obj) = pair
            for {
              s <- JenaMapper.jenaNode2RDFNode(subj)
              o <- JenaMapper.jenaNode2RDFNode(obj)
            } yield (s, o)
          })
          val r: IO[List[(RDFNode, RDFNode)]] = sequence(v)
          r
        }
      } yield pairs
    )

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] = {
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(subj, model, base)
    streamFromIOs(for {
      jenaPath <- JenaMapper.path2JenaPath(path, model, base)
      nodes <- sequence(
        JenaUtils.objectsFromPath(jenaNode, jenaPath, model).toList.map(n => JenaMapper.jenaNode2RDFNode(n))
      )
    } yield nodes)
  }

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] = {
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(obj, model, base)
    streamFromIOs(for {
      jenaPath <- JenaMapper.path2JenaPath(path, model, base)
      nodes <- sequence(
        JenaUtils.subjectsFromPath(jenaNode, jenaPath, model).toList.map(n => JenaMapper.jenaNode2RDFNode(n))
      )
    } yield nodes)
  }

  private def toRDFTriples(ls: Set[Statement]): IO[List[RDFTriple]] = {
    sequence(ls.toList.map(st => statement2triple(st)))
  }

  override def triplesWithPredicate(node: IRI): RDFStream[RDFTriple] =
    streamFromIOs(for {
      pred <- rdfNode2Property(node, model, base)
      ss   <- triplesPredicate(pred, model)
      ts   <- toRDFTriples(ss)
    } yield ts)

  override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = {
    streamFromIOs(for {
      r  <- rdfNode2Resource(node, model, base)
      ss <- triplesObject(r, model)
      ts <- toRDFTriples(ss)
    } yield ts)
  }

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] =
    streamFromIOs(for {
      pred <- rdfNode2Property(p, model, base)
      obj  <- rdfNode2Resource(o, model, base)
      ss   <- triplesPredicateObject(pred, obj, model)
      ts   <- toRDFTriples(ss)
    } yield ts)

  private def model2triples(model: Model): IO[List[RDFTriple]] = {
    model.listStatements.asScala.map(st => statement2triple(st)).toList.sequence
  }

  private def statement2triple(st: Statement): IO[RDFTriple] =
    for {
      subj <- JenaMapper.jenaNode2RDFNode(st.getSubject)
      obj  <- JenaMapper.jenaNode2RDFNode(st.getObject)
    } yield RDFTriple(subj, property2iri(st.getPredicate), obj)

  private def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  override def getPrefixMap: PrefixMap = {
    PrefixMap(model.getNsPrefixMap.asScala.toMap.map {
      case (alias, iri) => (Prefix(alias), IRI(iri))
    })
  }

  def addBase(iri: IRI): Rdf = {
    this.copy(base = Some(iri))
  }

  override def addPrefixMap(other: PrefixMap): Rdf = {
    val newMap = getPrefixMap.addPrefixMap(other)
    val map: Map[String, String] = newMap.pm.map {
      case (Prefix(str), iri) => (str, iri.str)
    }
    RDFAsJenaModel(model.setNsPrefixes(map.asJava))
  }

  // TODO: Check that the last character is indeed :
  // private def removeLastColon(str: String): String = str.init

  override def addTriples(triples: Set[RDFTriple]): Either[String, Rdf] = {
    Try {
      val newModel = JenaMapper.RDFTriples2Model(triples, model, base)
      val m        = model.add(newModel)
      RDFAsJenaModel(m)
    }.fold(e => s"Error adding triples: ${e.getMessage}".asLeft[Rdf], Right(_))
  }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): Either[String, Rdf] = {
    val empty        = ModelFactory.createDefaultModel
    val model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty, base)
    model.difference(model2delete)
    Right(this)
  }

  override def createBNode: (RDFNode, RDFAsJenaModel) = {
    val resource = model.createResource
    (BNode(resource.getId.getLabelString), this)
  }

  override def addPrefix(alias: String, iri: IRI): Rdf = {
    model.setNsPrefix(alias, iri.str)
    this
  }

  /*  private def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  } */

  override def empty: RDFRead[Rdf] = {
    RDFAsJenaModel.empty
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String, Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  /*private def resolveString(str: String): Either[String,IRI] = {
    Try(IRIResolver.resolveString(str)).fold(
      e => Left(e.getMessage),
      iri => Right(IRI(iri))
    )
  }*/
  private val NONE = "NONE"
  private val RDFS = "RDFS"
  private val OWL  = "OWL"

  override def applyInference(inference: String): Either[String, Rdf] = {
    inference.toUpperCase match {
      case `NONE` => Right(this)
      case `RDFS` => JenaUtils.inference(model, RDFS).map(RDFAsJenaModel(_))
      case `OWL`  => JenaUtils.inference(model, OWL).map(RDFAsJenaModel(_))
      case other  => Left(s"Unsupported inference $other")
    }
  }

  def availableInferenceEngines: List[String] = List(NONE, RDFS, OWL)

  override def querySelect(queryStr: String): RDFRead[List[Map[String, RDFNode]]] =
    Try {
      val qExec = QueryExecutionFactory.create(queryStr, model)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result = qExec.execSelect()
          val ls: List[IO[Map[String, RDFNode]]] = result.asScala.toList.map(qs => {
            val qsm = new QuerySolutionMap()
            qsm.addAll(qs)
            val pairs: List[(String, JenaRDFNode)] =
              qsm.asMap.asScala.view.toMap.toList
            val iom: IO[Map[String, RDFNode]] =
              pairs
                .map {
                  case (v, jenaNode) => jenaNode2RDFNode(jenaNode).flatMap(node => ok((v, node)))
                }
                .sequence
                .map(_.toMap)
            iom
          })
          ls.sequence
        }
        case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
      }
    }.fold(IO.raiseError(_), identity)

  override def queryAsJson(queryStr: String): Either[String, Json] =
    Try {
      val qExec = QueryExecutionFactory.create(queryStr, model)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result       = qExec.execSelect()
          val outputStream = new ByteArrayOutputStream()
          ResultSetFormatter.outputAsJSON(outputStream, result)
          val jsonStr = new String(outputStream.toByteArray())
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

  override def getNumberOfStatements(): Either[String, Int] =
    Right(model.size.toInt)

  override def isIsomorphicWith(other: RDFReader): Either[String, Boolean] = other match {
    case o: RDFAsJenaModel => {
      Right(model.isIsomorphicWith(o.model))
    }
    case _ => Left(s"Cannot compare RDFAsJenaModel with reader of different type: ${other.getClass.toString}")
  }

  /* def normalizeBNodes: Rdf = {
    NormalizeBNodes.normalizeBNodes(this, this.empty)
  } */

  /**
    * Apply owl:imports closure to an RDF source
    * @return new RDFReader
    */
  override def extendImports(): RDFBuild[Rdf] =
    for {
      imports <- getImports
      newRdf  <- extendImports(this, imports, List(IRI("")))
    } yield newRdf

  private lazy val owlImports = IRI("http://www.w3.org/2002/07/owl#imports")

  private def getImports: RDFBuild[List[IRI]] =
    for {
      ts <- triplesWithPredicate(owlImports).compile.toList
      os <- fromES(ts.map(_.obj.toIRI).sequence)
    } yield os

  private def extendImports(rdf: Rdf, imports: List[IRI], visited: List[IRI]): RDFBuild[Rdf] = {
    imports match {
      case Nil => ok(rdf)
      case iri :: rest =>
        if (visited contains iri)
          extendImports(rdf, rest, visited)
        else
          for {
            newRdf  <- RDFAsJenaModel.fromIRI(iri)
            merged  <- merge(newRdf)
            restRdf <- extendImports(merged, rest, iri :: visited)
          } yield restRdf
    }
  }

  override def asRDFBuilder: RDFRead[RDFBuilder] =
    err(s"Not implemented asRDFBuilder") // Right(this)

  override def rdfReaderName: String = s"ApacheJena"

  def addModel(model: Model): RDFAsJenaModel =
    this.copy(model = this.model.add(model))

  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] =
    err(s"Not implemented yet hasPredicateWithSubject")

  override def merge(other: RDFReader): RDFBuild[RDFAsJenaModel] = err(s"Not implemented yet merge")

  /*override def merge(other: RDFReader): Either[String, Rdf] = other match {
    case jenaRdf: RDFAsJenaModel =>
      Right(RDFAsJenaModel(this.model.add(jenaRdf.normalizeBNodes.model)))
    case _ => {
      val zero: Either[String, Rdf] = Right(this)
      def cmb(next: Either[String, Rdf], x: RDFTriple): Either[String, Rdf] =
        for {
          rdf1 <- next
          rdf2 <- rdf1.addTriple(x)
        } yield rdf2
      for {
        ts  <- other.rdfTriples
        rdf <- ts.foldLeft(zero)(cmb)
      } yield rdf
    }
  }*/

}

object RDFAsJenaModel {

  /*  def apply(): RDFAsJenaModel = {
    RDFAsJenaModel.empty
  } */

  def empty: IO[RDFAsJenaModel] = {
    IO(RDFAsJenaModel(ModelFactory.createDefaultModel))
  }

  def fromIRI(iri: IRI): IO[RDFAsJenaModel] = {
    Try {
      // We delegate RDF management to Jena (content-negotiation and so...)
      // RDFDataMgr.loadModel(iri.str)
      val m = ModelFactory.createDefaultModel()
      //      RDFDataMgr.read(m, uri, baseURI, shortnameToLang(format))
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      val ctx: Context    = null
      RDFParser.create.source(iri.str).labelToNode(LabelToNode.createUseLabelEncoded()).context(ctx).parse(dest)
      m
    }.fold(e => IO.raiseError(e), model => IO(RDFAsJenaModel(model)))
  }

  def fromURI(uri: String, format: String = "TURTLE", base: Option[IRI] = None): IO[RDFAsJenaModel] = {
    val baseURI = base.getOrElse(IRI(FileUtils.currentFolderURL))
    Try {
      val m = ModelFactory.createDefaultModel()
//      RDFDataMgr.read(m, uri, baseURI, shortnameToLang(format))
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      val ctx: Context    = null
      RDFParser.create
        .source(uri)
        .base(baseURI.str)
        .labelToNode(LabelToNode.createUseLabelEncoded())
        .lang(shortnameToLang(format))
        .context(ctx)
        .parse(dest)
      // RDFAsJenaModel(JenaUtils.relativizeModel(m), Some(IRI(uri)))
      RDFAsJenaModel(m, base, Some(IRI(uri)))
    }.fold(e => IO.raiseError(e), IO(_))
  }

  def fromFile(file: File, format: String, base: Option[IRI] = None): IO[RDFAsJenaModel] = {
    val baseURI = base.getOrElse(IRI(""))
    Try {
      val m               = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      // RDFDataMgr.read(m, is, baseURI, shortnameToLang(format))
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      val ctx: Context    = null
      RDFParser.create
        .source(is)
        .base(baseURI.str)
        .labelToNode(LabelToNode.createUseLabelEncoded())
        .lang(shortnameToLang(format))
        .context(ctx)
        .parse(dest)

      // RDFAsJenaModel(JenaUtils.relativizeModel(m), Some(IRI(file.toURI)))
      RDFAsJenaModel(m, base, Some(IRI(file.toURI)))
    }.fold(e => IO.raiseError(e), IO(_))
  }

  def fromString(str: String, format: String, base: Option[IRI] = None): IO[RDFAsJenaModel] = {
    fromChars(str, format, base)
  }

  def fromChars(cs: CharSequence, format: String, base: Option[IRI] = None): IO[RDFAsJenaModel] =
    for {
      empty  <- RDFAsJenaModel.empty
      newRdf <- empty.fromString(cs, format, base)
    } yield newRdf

  def extractModel(rdf: RDFAsJenaModel): Model = {
    rdf match {
      case rdfJena: RDFAsJenaModel => rdfJena.model
      case _                       => throw new Exception("Cannot extract Model from rdf:" + rdf)
    }
  }

  def availableFormats: List[String] = {
    RDFLanguages.getRegisteredLanguages().asScala.map(_.getName).toList.distinct
  }

}
