package es.weso.rdf.rdf4j

import java.io._
import es.weso.rdf._
import es.weso.rdf.path.SHACLPath
import es.weso.rdf.triples.RDFTriple
import io.circe.Json
import org.eclipse.rdf4j.model.{
  IRI => IRI_RDF4j,
  BNode => _,
  Literal => _,
  Resource => RDF4jResource,
  _
}
import es.weso.rdf.nodes.{IRI, _}
import org.eclipse.rdf4j.model.util.{ModelBuilder, Models}
import org.eclipse.rdf4j.rio.RDFFormat._
import org.eclipse.rdf4j.rio.{RDFFormat, Rio}
import org.apache.commons.io.input.CharSequenceInputStream
import scala.util._
// import scala.jdk.CollectionConverters._
// import es.weso.utils.internal.CollectionCompat
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import RDF4jMapper._
import cats.effect._
import cats._
import cats.implicits._
import fs2.Stream
import es.weso.utils.IOUtils._

case class RDFAsRDF4jModel(model: Model, base: Option[IRI] = None, sourceIRI: Option[IRI] = None)
    extends RDFReader
    with RDFBuilder
    with RDFReasoner {

  val id = s"RDFAsRDF4jModel($sourceIRI)"

  type Rdf = RDFAsRDF4jModel

  override def availableParseFormats: List[String]     = RDFAsRDF4jModel.availableFormats
  override def availableSerializeFormats: List[String] = RDFAsRDF4jModel.availableFormats

/*  override def fromString(cs: CharSequence, format: String, base: Option[IRI] = None): IO[Rdf] =
  fromES {
    val baseURI = base.map(_.str).getOrElse("")
    for {
      format <- getRDFFormat(format)
      model <- Try {
        val is: InputStream = new CharSequenceInputStream(cs, "UTF-8")
        Rio.parse(is, baseURI, format)
      }.fold(e => Left(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs"), Right(_))
    } yield RDFAsRDF4jModel(model)
  } */


  override def hasPredicateWithSubject(n: RDFNode, p: IRI): IO[Boolean] = 
   triplesWithSubjectPredicate(n,p).compile.toList.map(!_.isEmpty)


  override def serialize(formatName: String, base: Option[IRI]): IO[String] =
    fromES(for {
      format <- RDFAsRDF4jModel.getRDFFormat(formatName)
      str <- Try {
        val out: StringWriter = new StringWriter()
        // TODO: relitivize model according to base
        Rio.write(model, out, format)
        out.toString
      }.fold(e => Left(s"Error serializing RDF to format $formatName: $e"), Right(_))
    } yield str)

  /*  private def extend_rdfs: Rdf = {
    this
    // TODO: Check how to add inference in RDF4j
    /* val infModel = ModelFactory.createRDFSModel(model)
    RDFAsJenaModel(infModel) */
  } */

  // TODO: this implementation only returns subjects
  override def iris(): RDFStream[IRI] = {
    val resources: Set[RDF4jResource] = model.subjects().asScala.toSet
    val ls = resources.filter(_.isInstanceOf[IRI_RDF4j]).map(_.asInstanceOf[IRI_RDF4j].toString).map(IRI(_)).toList
    Stream.emits(ls)
  }

  override def subjects(): RDFStream[RDFNode] = streamFromIOs(for {
    resources <- IO(model.subjects().asScala.toSet)
    nodes <- resources.toList.map(r => resource2RDFNode(r)).sequence
  } yield nodes)

  override def rdfTriples(): RDFStream[RDFTriple] = {
    streamFromIOs(model.asScala.toSet.map(statement2RDFTriple(_)).toList.sequence)
  }

  override def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] =
    streamFromIOs(for {
      resource <- rdfNode2Resource(node)
      statements <- triplesSubject(resource, model)
      triples <- statements2RDFTriples(statements)
    } yield triples
    )

  /**
    * return the SHACL instances of a node `cls`
    * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
    */
  override def getSHACLInstances(c: RDFNode): RDFStream[RDFNode] = {
    streamFromIOs(RDF4jUtils.getSHACLInstances(c, model).map(_.toList))
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): RDFRead[Boolean] = {
    RDF4jUtils.getSHACLInstances(c, model).map(_ contains (n))
  }

  override def nodesWithPath(path: SHACLPath): RDFStream[(RDFNode, RDFNode)] = {
    /*
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val pairs = JenaUtils.getNodesFromPath(jenaPath, model).
      map(p => (JenaMapper.jenaNode2RDFNode(p._1), JenaMapper.jenaNode2RDFNode(p._2)))
    pairs.toSet */
    errStream(s"nodesWithPath: not implemented yet")
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode] = {
    streamFromIOs(RDF4jUtils.objectsWithPath(subj, path, model).map(_.toList))
  }

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): RDFStream[RDFNode] = {
    streamFromIOs(RDF4jUtils.subjectsWithPath(obj, path, model).map(_.toList))
  }

  override def triplesWithPredicate(iri: IRI): RDFStream[RDFTriple] = {
    val pred = iri2Property(iri)
    streamFromIOs(for {
      statements <- triplesPredicate(pred, model)
      triples <- statements2RDFTriples(statements)
    } yield triples)
  }

  override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = streamFromIOs(
   for {
    either <- MonadError[IO,Throwable].attempt(rdfNode2Resource(node))
    ts <- either.fold(_ => emptyTriples, 
     resource => for { 
       triples <- triplesObject(resource,model)
       statements <- statements2RDFTriples(triples)
      } yield statements
    )
   } yield ts
  )

  private lazy val emptyTriples: IO[List[RDFTriple]] = IO(List())

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple] = streamFromIOs( 
   for {
    either <- MonadError[IO,Throwable].attempt(rdfNode2Resource(o))
    ts <- either.fold(_ => emptyTriples, 
     resource => { 
     val v: IO[List[RDFTriple]] = for {
      triples <- triplesPredicateObject(iri2Property(p), resource, model)
      statements <- statements2RDFTriples(triples)
     } yield statements 
     v
    }
    )
   } yield ts
   )

  override def getPrefixMap: IO[PrefixMap] = {
    IO(PrefixMap {
      val nsSet: Set[Namespace] = model.getNamespaces.asScala.toSet
      nsSet.map(ns => (Prefix(ns.getPrefix), IRI(ns.getName))).toMap
    })
  }

  override def addBase(iri: IRI): IO[Rdf] = {
    IO.pure(this.copy(base = Some(iri)))
  } 

  override def addPrefixMap(pm: PrefixMap): IO[Rdf] = IO {
    pm.pm.foreach {
      case (Prefix(prefix), value) => model.setNamespace(prefix, value.str)
    }
    this
  }

  override def addTriples(triples: Set[RDFTriple]): IO[Rdf] =
    for {
      statements <- triples.map(rdfTriple2Statement(_)).toList.sequence
    } yield {
      // val xs: List[Statement] = statements
      model.addAll(statements.asJava)
      this
    }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): IO[Rdf] =
    for {
      s <- rdfTriple2Statement(triple)
      _ <- IO { model.remove(s) }
    } yield this

  override def createBNode: IO[(RDFNode, Rdf)] = IO {
    (BNode(newBNode.getID), this)
  }

  override def addPrefix(alias: String, iri: IRI): IO[Rdf] = IO {
    model.setNamespace(alias, iri.str)
    this
  }

  override def empty: IO[Resource[IO,Rdf]] = {
    // TODO: Refactor to avoid unsafeRunSync
    IO(RDFAsRDF4jModel.empty)
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): IO[Boolean] =
    wellTypedDatatype(node, datatype)

  /*private def resolveString(str: String): Either[String,IRI] = {
    Try(IRIResolver.resolveString(str)).fold(
      e => Left(e.getMessage),
      iri => Right(IRI(iri))
    )
  }*/
  private val NONE = "NONE"
  private val RDFS = "RDFS"
  private val OWL  = "OWL"

  override def applyInference(inference: String): IO[Rdf] = {
    ok(this) // TODO (as it is doesn't apply inference)
    /*
    inference.toUpperCase match {
      case `NONE` => Right(this)
      case `RDFS` => JenaUtils.inference(model, RDFS).map(RDFAsJenaModel(_))
      case `OWL` => JenaUtils.inference(model, OWL).map(RDFAsJenaModel(_))
      case other => Left(s"Unsupported inference $other")
    }
   */
  }

  override def availableInferenceEngines: List[String] = List(NONE, RDFS, OWL)

  override def querySelect(queryStr: String): RDFStream[Map[String, RDFNode]] =
        Stream.raiseError[IO](new RuntimeException(s"Not implemented querySelect for RDf4j yet"))

  override def queryAsJson(queryStr: String): IO[Json] =
    err(s"Not implemented queryAsJson for RDf4j")

  override def getNumberOfStatements(): IO[Int] =
    ok(model.size)

  def isIsomorphicWith(other: RDFReader): IO[Boolean] = other match {
    case o: RDFAsRDF4jModel => ok(Models.isomorphic(model, o.model))
    case _                  => err(s"Cannot compare RDFAsJenaModel with reader of different type: ${other.getClass.toString}")
  }

  override def merge(other: RDFReader): IO[Rdf] = other match {
    // TODO: optimize merge using RDF4j merge...
    // case rdf4j: RDFAsRDF4jModel =>
    case _ => {
      val zero: IO[Rdf] = ok(this)
      def cmb(next: IO[Rdf], x: RDFTriple): IO[Rdf] =
        for {
          rdf1 <- next
          rdf2 <- rdf1.addTriple(x)
        } yield rdf2

      for {
        ts  <- other.rdfTriples.compile.toList
        rdf <- ts.foldLeft(zero)(cmb)
      } yield rdf
    }
  }

  override def extendImports: IO[Rdf] =
    for {
      imports <- getImports
      newRdf  <- extendImports(this, imports, List(IRI("")))
    } yield newRdf

  private lazy val owlImports = IRI("http://www.w3.org/2002/07/owl#imports")

  private def getImports: IO[List[IRI]] =
    for {
      ts <- triplesWithPredicate(owlImports).compile.toList
      is <- fromES(ts.map(_.obj).map(_.toIRI).toList.sequence)
    } yield is

  private def extendImports(rdf: Rdf, imports: List[IRI], visited: List[IRI]): IO[Rdf] = {
    imports match {
      case Nil => ok(rdf)
      case iri :: rest =>
        if (visited contains iri)
          extendImports(rdf, rest, visited)
        else
          for {
            newRdf  <- RDFAsRDF4jModel.fromIRI(iri)
            merged  <- merge(newRdf)
            restRdf <- extendImports(merged, rest, iri :: visited)
          } yield restRdf
    }
  }

  override def asRDFBuilder: IO[RDFBuilder] =
    ok(this)

  override def rdfReaderName: String = s"RDF4j"

  // TODO: This method is not implemented yet
  override def normalizeBNodes: IO[RDFBuilder] =
    IO(this)

  override def fromString(str: String, format: String, base: Option[IRI]): IO[Resource[IO, Rdf]] =
    IO(RDFAsRDF4jModel.fromChars(str,format,base))
}

object RDFAsRDF4jModel {

  def closeRDFjModel(m: RDFAsRDF4jModel): IO[Unit] = IO {
    // It seems there is no close() method in RDF4j
  }

  def apply(): Resource[IO,RDFAsRDF4jModel] = {
    empty
  }

  lazy val empty: Resource[IO,RDFAsRDF4jModel] = {
    val acquire: IO[RDFAsRDF4jModel] = IO {
      val builder = new ModelBuilder()
      RDFAsRDF4jModel(builder.build)
    }
    Resource.make(acquire)(closeRDFjModel)
  }

  def fromChars(cs: CharSequence, format: String, base: Option[IRI] = None): Resource[IO,RDFAsRDF4jModel] = {
    val acquire: IO[RDFAsRDF4jModel] =
      fromES {
        val baseURI = base.map(_.str).getOrElse("")
        for {
          format <- getRDFFormat(format)
          model <- Try {
            val is: InputStream = new CharSequenceInputStream(cs, "UTF-8")
            Rio.parse(is, baseURI, format)
          }.fold(
            e => Left(s"Exception obtaining RDF: ${e.getMessage}\nBase:$base, format: $format\n$cs"),
            Right(_)
          )
        } yield RDFAsRDF4jModel(model)
      }
    Resource.make(acquire)(closeRDFjModel)
  }

  def availableFormats: List[String] = {
    val formats = List(TURTLE, JSONLD, RDFXML)
    formats.map(_.getName)
  }

  def fromIRI(iri: IRI): IO[RDFAsRDF4jModel] = {
    err(s"Not implemented get RDF4j from IRI: $iri")
  }

  private def getRDFFormat(name: String): Either[String, RDFFormat] = {
    name.toUpperCase match {
      case "TURTLE" => Right(TURTLE)
      case "JSONLD" => Right(JSONLD)
      case "RDFXML" => Right(RDFXML)
      case x        => Left(s"Unsupported syntax $x")
    }
  }


}
