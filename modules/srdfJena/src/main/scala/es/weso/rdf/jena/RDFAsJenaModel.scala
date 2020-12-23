package es.weso.rdf.jena
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import org.apache.jena.graph.Node
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import org.apache.jena.graph.Triple
import scala.util.Try
import es.weso.rdf._
import org.apache.jena.rdf.model.{Model, Statement, RDFNode => JenaRDFNode, Resource => JenaResource}
import org.slf4j._
import org.apache.jena.riot._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.lang._
import scala.util._
import java.io._
import cats.effect.concurrent.Ref
import org.apache.jena.riot.RDFLanguages._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.jena.SRDFJenaException.{FromFileException, FromStringException, FromUriException, UnsupportedFormat}
import es.weso.rdf.path.SHACLPath
import es.weso.utils._
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.query.{Query, QueryExecutionFactory, QuerySolutionMap, ResultSetFormatter}
//import cats.implicits._
import org.apache.jena.graph.Graph
import org.apache.jena.riot.system.{StreamRDF, StreamRDFLib}
import org.apache.jena.sparql.util.Context
import cats.effect._
import es.weso.utils.IOUtils._
import cats.implicits._
import fs2.Stream
import es.weso.utils.StreamUtils._
import org.apache.jena.riot.system.SyntaxLabels
import org.apache.jena.riot.system.ParserProfileStd
import org.apache.jena.riot.system.ParserProfile
import org.apache.jena.riot.system.RiotLib
import org.apache.jena.riot.system.ErrorHandlerFactory
import org.apache.jena.riot.system.IRIResolver
import org.apache.jena.riot.system.PrefixMapFactory
import org.apache.jena.riot.tokens.Token
import es.weso.rdf.locations._
import java.net.URL

case class RDFAsJenaModel(modelRef: Ref[IO,Model],
                          base: Option[IRI],
                          sourceIRI: Option[IRI],
                          nodesLocations: Map[RDFNode,Set[Location]],
                          triplesLocations: Map[RDFTriple,Set[Location]]
 ) extends LocationsReader(nodesLocations, triplesLocations)
   with RDFReader
   with RDFBuilder
   with RDFReasoner {

  val id = s"RDFAsJenaModel($sourceIRI)"

  def getModel: IO[Model] = modelRef.get

  type Rdf = RDFAsJenaModel

  val log: Logger = LoggerFactory.getLogger("RDFAsJenaModel")

  def availableParseFormats: List[String]     = RDFAsJenaModel.availableFormats
  def availableSerializeFormats: List[String] = RDFAsJenaModel.availableFormats

  private def getRDFFormat(formatName: String): IO[String] = {
    val supportedFormats: List[String] =
      RDFLanguages.getRegisteredLanguages().asScala.toList.map(_.getName.toUpperCase).distinct
    formatName.toUpperCase match {
      case format if supportedFormats.contains(format) => format.pure[IO]
      case _ => IO.raiseError(UnsupportedFormat(formatName))
    }
  }

  override def serialize(formatName: String, base: Option[IRI]): RDFRead[String] =
    for {
        format <- getRDFFormat(formatName)
        model <- getModel
        str = {
          val out              = new ByteArrayOutputStream()
          val relativizedModel = JenaUtils.relativizeModel(model, base.map(_.uri))
          relativizedModel.write(out, format)
          out.toString
        }
    } yield str

  // TODO: this implementation only returns subjects
  override def iris(): RDFStream[IRI] = for {
    model <- Stream.eval(getModel)
    resources: Seq[JenaResource] = model.listSubjects().asScala.toSeq
    iris <- Stream.emits(resources.filter(s => s.isURIResource).map(r => IRI(r.getURI)))
  } yield iris

  override def subjects(): RDFStream[RDFNode] = for {
    model <- Stream.eval(getModel)
    resources: Set[JenaResource] = model.listSubjects().asScala.toSet
    ss <- streamFromIOs(sequence(resources.map(r => jenaNode2RDFNode(r)).toList).map(_.toList))
  } yield ss

  override def rdfTriples(): RDFStream[RDFTriple] = for {
    model <- Stream.eval(getModel)
    ts <- streamFromIOs(model2triples(model))
  } yield ts

  override def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case n if n.isLiteral => Stream.empty
    case _ =>
      streamFromIOs(for {
        model <- getModel
        resource   <- rdfNode2Resource(node, model, base)
        statements <- triplesSubject(resource, model)
        ts         <- toRDFTriples(statements)
      } yield ts)
  }

  override def triplesWithSubjectPredicate(node: RDFNode, p: IRI): RDFStream[RDFTriple] = {
    if (node.isLiteral) Stream.empty
    else streamFromIOs(for {
      model <- getModel
      r  <- rdfNode2Resource(node, model, base)
      ss <- triplesSubjectPredicate(r, p, model, base)
      ts <- toRDFTriples(ss)
    } yield ts)
  }

  override def fromString(str: String, format: String, base: Option[IRI]): IO[Resource[IO, Rdf]] =
    RDFAsJenaModel.fromString(str,format,base)

  /**
    * return the SHACL instances of a node `cls`
    * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
    */
  override def getSHACLInstances(c: RDFNode): RDFStream[RDFNode] =
    streamFromIOs(for {
      model <- getModel
      is <- JenaUtils.getSHACLInstances(JenaMapper.rdfNode2JenaNode(c, model, base), model)
      ns <- is.toList.map(n => JenaMapper.jenaNode2RDFNode(n)).sequence
    } yield ns)

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = for {
    model <- getModel
    nJena = JenaMapper.rdfNode2JenaNode(n, model, base)
    cJena = JenaMapper.rdfNode2JenaNode(c, model, base)
    b <- JenaUtils.hasClass(nJena, cJena, model)
  } yield b

  override def nodesWithPath(path: SHACLPath): RDFStream[(RDFNode, RDFNode)] =
    streamFromIOs(
      for {
        model <- getModel
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
    streamFromIOs(for {
      model <- getModel
      jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(subj, model, base)
      jenaPath <- JenaMapper.path2JenaPath(path, model, base)
      nodes <- sequence(
        JenaUtils.objectsFromPath(jenaNode, jenaPath, model).toList.map(n => JenaMapper.jenaNode2RDFNode(n))
      )
    } yield nodes)
  }

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] = {
    streamFromIOs(for {
      model <- getModel
      jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(obj, model, base)
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
      model <- getModel
      pred <- rdfNode2Property(node, model, base)
      ss   <- triplesPredicate(pred, model)
      ts   <- toRDFTriples(ss)
    } yield ts)

  override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = {
    streamFromIOs(for {
      model <- getModel
      r  = rdfNode2JenaNode(node, model, base)
      ss <- triplesObject(r, model)
      ts <- toRDFTriples(ss)
    } yield ts)
  }

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] = {

    streamFromIOs(for {
      model <- getModel
      obj = rdfNode2JenaNode(o, model, base)
      pred <- rdfNode2Property(p, model, base)
      ss   <- triplesPredicateObject(pred, obj, model)
      ts   <- toRDFTriples(ss)
    } yield ts)
  }

  override def getPrefixMap: RDFRead[PrefixMap] = for {
    model <- getModel
    pm = PrefixMap(model.getNsPrefixMap.asScala.toMap.map {
      case (alias, iri) => (Prefix(alias), IRI(iri))
    })
  } yield pm

  override def addBase(iri: IRI): IO[Rdf] = {
    IO.pure(this.copy(base = Some(iri)))
  } 

  override def addPrefixMap(other: PrefixMap): IO[Rdf] = for {
    model <- getModel
    pm <- getPrefixMap
    newMap = pm.addPrefixMap(other)
    map: Map[String, String] = newMap.pm.map {
      case (Prefix(str), iri) => (str, iri.str)
    }
    rdf <- RDFAsJenaModel.fromModel(model.setNsPrefixes(map.asJava))
  } yield rdf

  // TODO: Check that the last character is indeed :
  // private def removeLastColon(str: String): String = str.init

  override def addTriples(triples: Set[RDFTriple]): IO[Rdf] = for {
    model <- getModel
    newModel = JenaMapper.RDFTriples2Model(triples, model, base)
    m        = model.add(newModel)
    newRdf <-  RDFAsJenaModel.fromModel(m)
  } yield newRdf

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): IO[Rdf] = for {
    model <- getModel
    empty        = ModelFactory.createDefaultModel
    model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty, base)
    rdf <- RDFAsJenaModel.fromModel(model.difference(model2delete))
  } yield rdf

  override def createBNode: IO[(RDFNode, RDFAsJenaModel)] = for {
    model <- getModel
    resource = model.createResource
  } yield (BNode(resource.getId.getLabelString), this)

  override def addPrefix(alias: String, iri: IRI): IO[Rdf] = for {
    model <- getModel
    rdf <- RDFAsJenaModel.fromModel(model.setNsPrefix(alias, iri.str))
  } yield rdf

  /*  private def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  } */

  override def empty: RDFRead[Resource[RDFRead,Rdf]] = {
    RDFAsJenaModel.empty
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  /*private def resolveString(str: String): Either[String,IRI] = {
    Try(IRIResolver.resolveString(str)).fold(
      e => Left(e.getMessage),
      iri => Right(IRI(iri))
    )
  }*/

  override def applyInference(inference: InferenceEngine): IO[Rdf] = {
    inference match {
      case NONE => ok(this)
      case RDFS => for {
        model <- getModel
        newModel <- JenaUtils.inference(model, inference.name)
        rdf <- RDFAsJenaModel.fromModel(newModel)
      } yield rdf

      case OWL  => for {
        model <- getModel
        newModel <- JenaUtils.inference(model, inference.name)
        rdf <- RDFAsJenaModel.fromModel(newModel)
      } yield rdf
      case other  => err(s"Unsupported inference ${other.name}")
    }
  }

  override def querySelect(queryStr: String): RDFStream[Map[String, RDFNode]] = for {
    model <- Stream.eval(getModel)
    stream <- Try {
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
    }.fold(Stream.raiseError[IO], fromIOLs)
  } yield stream

  override def queryAsJson(queryStr: String): IO[Json] = for {
    model <- getModel
    stream <-     Try {
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
    }.fold(f => err(f.getMessage), _.fold(err(_), ok(_)))
  } yield stream

  override def getNumberOfStatements(): IO[Int] = for {
    model <- getModel
  } yield model.size.toInt

  override def isIsomorphicWith(other: RDFReader): IO[Boolean] = other match {
    case o: RDFAsJenaModel => for {
      model <- getModel
      otherModel <- o.getModel
    } yield model.isIsomorphicWith(otherModel)
    case _ => err(s"Cannot compare RDFAsJenaModel with reader of different type: ${other.getClass.toString}")
  }

  override def normalizeBNodes: RDFBuild[RDFBuilder] = // IO(this)
   for {
    // e <- RDFAsJenaModel.empty
    normalized <- normalizeBNodesJena(this)
  } yield normalized

  private def normalizeBNodesJena(rdf: RDFAsJenaModel): IO[RDFBuilder] =
    NormalizeBNodes.normalizeBNodes(rdf,rdf)

  /**
    * Apply owl:imports closure to an RDF source
    * @return new RDFReader
    */
  override def extendImports: RDFBuild[Rdf] =
    for {
      imports <- getImports
      // _ <- IO { println(s"Imports: ${imports}") }
      newRdf  <- extendImports(this, imports, List(IRI("")), base)
    } yield newRdf

  private lazy val owlImports = IRI("http://www.w3.org/2002/07/owl#imports")

  private def getImports: RDFBuild[List[IRI]] =
    for {
      ts <- triplesWithPredicate(owlImports).compile.toList
      os <- fromES(ts.map(_.obj.toIRI).sequence)
    } yield os

  private def extendImports(rdf: Rdf,
                            imports: List[IRI],
                            visited: List[IRI],
                            base: Option[IRI]
                           ): RDFBuild[Rdf] = {
    imports match {
      case Nil => ok(rdf)
      case iri :: rest =>
        if (visited contains iri)
          extendImports(rdf, rest, visited, base)
        else for {
          res <- RDFAsJenaModel.fromIRI(iri, base = base)
          rdf <- res.use(newRdf => for {
              merged  <- merge(newRdf)
              restRdf <- extendImports(merged, rest, iri :: visited, base)
            } yield restRdf)
        } yield rdf
    }
  }

  override def asRDFBuilder: RDFRead[RDFBuilder] =
    err(s"Not implemented asRDFBuilder") // Right(this)

  override def rdfReaderName: String = s"ApacheJena"

  def addModel(otherModel: Model): IO[RDFAsJenaModel] = for {
    model <- getModel
    rdf <- RDFAsJenaModel.fromModel(model.add(otherModel))
  } yield rdf

  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] =
    triplesWithSubjectPredicate(n,p).compile.toList.map(!_.isEmpty)

  override def merge(other: RDFReader): RDFBuild[RDFAsJenaModel] = 
   other match {
    case jenaRdf: RDFAsJenaModel => for {
      model <- getModel
      otherModel <- jenaRdf.getModel
      newRdf <- RDFAsJenaModel.fromModel(model.union(otherModel))
    } yield  newRdf
    case _ => {
      val zero: IO[Rdf] = ok(this)
      def cmb(next: IO[Rdf], x: RDFTriple): IO[Rdf] =
        for {
          rdf1 <- next
          rdf2 <- rdf1.addTriple(x)
        } yield rdf2
      for {
        ts  <- other.rdfTriples().compile.toList
        rdf <- ts.foldLeft(zero)(cmb)
      } yield rdf
    }
  }

  override def availableInferenceEngines: List[InferenceEngine] = List(NONE,RDFS,OWL)

}

object RDFAsJenaModel {

  def fromModel(model: Model,
                base: Option[IRI] = None,
                sourceIRI: Option[IRI] = None,
                nodeLocations: Map[RDFNode,Set[Location]] = Map(),
                tripleLocations: Map[RDFTriple,Set[Location]] = Map()
               ): IO[RDFAsJenaModel] = for {
    ref <- Ref.of[IO, Model](model)
  } yield RDFAsJenaModel(ref,base,sourceIRI,nodeLocations,tripleLocations)


  private def acquireRDF: IO[RDFAsJenaModel] = for {
      model <- IO(ModelFactory.createDefaultModel)
      rdf <- RDFAsJenaModel.fromModel(model)
  } yield rdf

  private def closeRDF(m: RDFAsJenaModel): IO[Unit] = for {
    model <- m.getModel
  } yield model.close()


  def empty: IO[Resource[IO,RDFAsJenaModel]] = {
    IO(Resource.make(acquireRDF)(closeRDF))
  }

  def fromIRI(iri: IRI, format: String = "TURTLE", base: Option[IRI] = None): IO[Resource[IO,RDFAsJenaModel]] = {
    // println(s"fromIRI: ${iri}")
    fromURI(iri.str, format,base)
  }

  def fromURI(uri: String,
              format: String = "TURTLE",
              base: Option[IRI] = None): IO[Resource[IO,RDFAsJenaModel]] = {
    val baseURI = base.getOrElse(IRI(FileUtils.currentFolderURL))
    // println(s"fromURI: ${uri}\nCurrent folder: ${FileUtils.currentFolderURL}")              
    val url: URL = new URL(uri)
    val urlReader: Reader = new InputStreamReader(url.openStream())
    fromReader(urlReader, format, Some(baseURI))
  }

  def fromFile(file: File, format: String, base: Option[IRI] = None): 
    IO[Resource[IO,RDFAsJenaModel]] = {
    val is: InputStream = new FileInputStream(file)
    val fileReader: Reader = new InputStreamReader(is)
    fromReader(fileReader,format,base)
  }

  def fromString(str: String,
                 format: String,
                 base: Option[IRI] = None,
                 useBNodeLabels: Boolean = true
                ): IO[Resource[IO,RDFAsJenaModel]] = {
   val str_reader = new StringReader(str)  
   // println(s"fromString BASE:${base}")                
   fromReader(str_reader, format, base, useBNodeLabels)              
  }

  def fromReader(reader: Reader,
                 format: String,
                 base: Option[IRI] = None,
                 useBNodeLabels: Boolean = true
                ): IO[Resource[IO,RDFAsJenaModel]] = {

    var nodeLocations = Map[RDFNode,Set[Location]]()  
    var tripleLocations =  Map[RDFTriple,Set[Location]]()
    // println(s"fromReader BASE=${base}")                

  
    def profile(baseUri: String, labelToNode: LabelToNode): ParserProfile = {
     def addNodeLocation(node: RDFNode, location: Location): Unit = {
      nodeLocations = nodeLocations.updatedWith(node) {
        case None => Some(Set(location))
        case Some(ls) => Some(ls + location)
      }
     }

     def addTripleLocation(triple: RDFTriple, location: Location): Unit = {
      tripleLocations = tripleLocations.updatedWith(triple) {
        case None => Some(Set(location))
        case Some(ls) => Some(ls + location)
      }
     }

     new ParserProfileStd(RiotLib.factoryRDF(labelToNode),
        ErrorHandlerFactory.errorHandlerStd,
        IRIResolver.create(baseUri),
        PrefixMapFactory.create(),
        RIOT.getContext(), false, false) {

      override def createTriple(subject: Node, predicate: Node, obj: Node, line: Long, col: Long): Triple = {
        val triple: Triple = super.createTriple(subject, predicate, obj, line, col)
        val rdfTriple: RDFTriple = JenaMapper.jenaTriple2Triple(subject,predicate,obj).unsafeRunSync()
        val location = Location(line, col, "TRIPLE")
        addTripleLocation(rdfTriple, location) 
        // println(s"Adding triple location: ${rdfTriple}@${location}\nMap ${tripleLocations}")
        triple
      }
      override def create(currentGraph: Node, token: Token): Node = {
        val node: Node = super.create(currentGraph, token)
        val line: Long = token.getLine()
        val col: Long = token.getColumn()
        val t: String = token.getType().name()
        val location : Location = Location(line,col,t)
        val rdfNode: RDFNode = node2RDFNode(node).unsafeRunSync()
        addNodeLocation(rdfNode,location)
        // println(s"Adding node location: ${rdfNode}@${location}. Node: ${node}")
        node
      }
     }
    }

    val acquire = Try {
      val m               = ModelFactory.createDefaultModel
      val baseURI         = base.getOrElse(IRI(""))
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      val ctx: Context    = null
      val labelToNodePolicy = 
        if (useBNodeLabels) LabelToNode.createUseLabelEncoded()
        else SyntaxLabels.createLabelToNode()

      val parser: ReaderRIOT = 
          RDFParserRegistry
          .getFactory(shortnameToLang(format))
          .create(
            shortnameToLang(format), 
            profile(baseURI.str, labelToNodePolicy)
          )
        parser.read(reader, baseURI.str, null, dest,ctx)  
        m 
      }.fold(
      e => IO.raiseError(FromStringException(s"Error parsing RDF: ${e.getMessage()}\nReader: ${reader}",e)),
      m => RDFAsJenaModel.fromModel(m,base,None,nodeLocations.toMap, tripleLocations.toMap) 
    )
    IO(Resource.make(acquire)(closeRDF))
  }

  def fromChars(cs: CharSequence,
                format: String,
                base: Option[IRI] = None): IO[Resource[IO,RDFAsJenaModel]] = {
    /*for {
      empty  <- RDFAsJenaModel.empty
      newRdf <- empty.fromString(cs, format, base)
    } yield newRdf*/
    fromString(cs.toString, format, base)
  }

  def availableFormats: List[String] = {
    RDFLanguages.getRegisteredLanguages().asScala.map(_.getName).toList.distinct
  }


}


