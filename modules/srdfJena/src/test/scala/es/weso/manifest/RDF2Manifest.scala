package es.weso.shextest.manifest
import cats.effect._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.utils.FileUtilsIO._
import es.weso.utils.FileUtils._
import ManifestPrefixes._
import cats.data._
import cats.implicits._
import scala.util._
import es.weso.rdf.parser._

case class ManifestContext(base: Option[IRI], derefIncludes: Boolean, visited: List[IRI])

class RDF2Manifest extends RDFParser with LazyLogging {

  type ManifestParser[A] = ReaderT[RDFParser,ManifestContext,A]
  def liftParser[A](parser: RDFParser[A]): ManifestParser[A] = ReaderT.liftF(parser)
  def getContext[A]: ManifestParser[ManifestContext] = 
     ReaderT.ask[RDFParser,ManifestContext]
  def local[A](fn: ManifestContext => ManifestContext, parser: ManifestParser[A]): ManifestParser[A] = {
     Kleisli.local(fn)(parser)
  }

  private def rdf2Manifest: ManifestParser[List[ShExManifest]] = 
    for {
      ctx <- getContext
      rdf <- liftParser(getRDF)
      candidates <- liftParser(fromEitherT(rdf.subjectsWithTypeIO(mf_Manifest)))
      _ <- liftParser(info(s"rdf2Manifest: candidates=${candidates.mkString(",")}"))
      nodes <- { 
        liftParser(parseNodes(candidates.toList, manifest.run(ctx)))
      }
    } yield nodes


  private def manifest: ManifestParser[ShExManifest] =
    for {
      maybeLabel   <- liftParser(stringFromPredicateOptional(rdfs_label))
      maybeComment <- liftParser(stringFromPredicateOptional(rdfs_comment))
      entries      <- liftParser(entries)
      is     <- includes
    } yield {
      ShExManifest(label = maybeLabel, comment = maybeComment, entries = entries.toList, includes = is)
    }

  private def entries: RDFParser[Seq[Entry]] =
    parsePropertyList(mf_entries, entry).map(_.toSeq)

  private def representationTest: RDFParser[RepresentationTest] =
    for {
      n         <- getNode
      name      <- stringFromPredicate(mf_name)
      statusIri <- iriFromPredicate(mf_status)
      shex      <- iriFromPredicate(sx_shex)
      ttl       <- iriFromPredicate(sx_ttl)
      json      <- iriFromPredicate(sx_json)
    } yield RepresentationTest(n, Status(statusIri), name, json, shex, ttl)

  private def negativeSyntax: RDFParser[NegativeSyntax] =
    for {
      n         <- getNode
      name      <- stringFromPredicate(mf_name)
      statusIri <- iriFromPredicate(mf_status)
      shex      <- iriFromPredicate(sx_shex)
    } yield NegativeSyntax(n, Status(statusIri), name, shex)

  private def negativeStructure: RDFParser[NegativeStructure] =
    for {
      n         <- getNode
      name      <- stringFromPredicate(mf_name)
      statusIri <- iriFromPredicate(mf_status)
      shex      <- iriFromPredicate(sx_shex)
    } yield NegativeStructure(n, Status(statusIri), name, shex)

  private def validateTest: RDFParser[Validate] =
    for {
      n          <- getNode
      name       <- stringFromPredicate(mf_name)
      statusIri  <- iriFromPredicate(mf_status)
      actionNode <- objectFromPredicate(mf_action)
      action     <- withNode(actionNode, parseManifestAction)
      resultNode <- objectFromPredicate(mf_result)
      result     <- withNode(resultNode, result)
      specRef    <- optional(iriFromPredicate(sht_specRef))
    } yield Validate(n, Status(statusIri), name, action, result, specRef)

  private def validationTest: RDFParser[ValidationTest] =
    for {
      n           <- getNode
      name        <- stringFromPredicate(mf_name)
      statusIri   <- iriFromPredicate(mf_status)
      traits      <- irisFromPredicate(sht_trait)
      comment     <- stringFromPredicate(rdfs_comment)
      actionNode  <- objectFromPredicate(mf_action)
      action      <- withNode(actionNode, parseAction)
      maybeResult <- iriFromPredicateOptional(mf_result)
    } yield ValidationTest(n, Status(statusIri), name, traits, comment, action, maybeResult)

  private def validationFailure: RDFParser[ValidationFailure] =
    for {
      n           <- getNode
      name        <- stringFromPredicate(mf_name)
      statusIri   <- iriFromPredicate(mf_status)
      traits      <- irisFromPredicate(sht_trait)
      comment     <- stringFromPredicate(rdfs_comment)
      actionNode  <- objectFromPredicate(mf_action)
      action      <- withNode(actionNode, parseAction)
      maybeResult <- iriFromPredicateOptional(mf_result)
    } yield ValidationFailure(n, Status(statusIri), name, traits, comment, action, maybeResult)

  private def parseAction: RDFParser[Action] =
    for {
      maybeFocus <- objectFromPredicateOptional(sht_focus)
      action <- maybeFocus match {
        case Some(focus) => focusAction(focus)
        case None        => mapResultAction
      }
    } yield action

  private def mapResultAction: RDFParser[MapResultAction] =
    for {
      data     <- iriFromPredicate(sht_data)
      schema   <- iriFromPredicate(sht_schema)
      shapeMap <- iriFromPredicate(sht_map)
    } yield MapResultAction(data, schema, shapeMap)

  private def focusAction(focus: RDFNode): RDFParser[FocusAction] =
    for {
      schema <- iriFromPredicate(sht_schema)
      data   <- iriFromPredicate(sht_data)
      shape  <- objectFromPredicateOptional(sht_shape)
//      maybeMap <- iriFromPredicateOptional(sht_map)
      shapeExterns <- iriFromPredicateOptional(sht_shapeExterns)
    } yield FocusAction(data, schema, focus, shape, shapeExterns)

  private def entry: RDFParser[Entry] =
    for {
      entryTypeUri <- rdfType
      entry <- entryTypeUri match {
        case `sht_RepresentationTest` => representationTest
        case `sht_NegativeSyntax`     => negativeSyntax
        case `sht_NegativeStructure`  => negativeStructure
        case `sht_Validate`           => validateTest
        case `sht_ValidationTest`     => validationTest
        case `sht_ValidationFailure`  => validationFailure
        case _                        => parseFail(s"Unsupported entry type: $entryTypeUri")
      }
    } yield entry

  private def iriDataFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_TURTLE` => Right("TURTLE")
      case _            => Left("Unexpected schema format: " + iri)
    }
  }

  private def iriSchemaFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_SHACLC` => Right("SHACLC")
      case `sht_TURTLE` => Right("TURTLE")
      case _            => Left("Unexpected schema format: " + iri)
    }
  }

  private def parseManifestAction: RDFParser[ManifestAction] =
    for {
      data <- optional(iriFromPredicate(sht_data))
      schema <- {
        iriFromPredicateOptional(sht_schema)
      }
      dataFormatIri      <- optional(iriFromPredicate(sht_data_format))
      dataFormat         <- mapOptional(dataFormatIri, iriDataFormat2str)
      schemaFormatIRI    <- optional(iriFromPredicate(sht_schema_format))
      schemaFormat       <- mapOptional(schemaFormatIRI, iriSchemaFormat2str)
      schemaOutputFormat <- optional(iriFromPredicate(sht_schema_output_format))
      triggerMode        <- optional(iriFromPredicate(sht_triggerMode))
      node               <- optional(oneOfPredicates(Seq(sht_node, sht_focus)))
      shape              <- optional(iriFromPredicate(sht_shape))
      focus              <- optional(iriFromPredicate(sht_focus))
      shapeMap           <- optional(iriFromPredicate(sht_shapeMap))
      resultShapeMap     <- optional(iriFromPredicate(sht_resultShapeMap))
    } yield ManifestAction(
      schema = schema,
      schemaFormat = schemaFormat,
      data = data,
      dataFormat = dataFormat,
      triggerMode = triggerMode,
      schemaOutputFormat = schemaOutputFormat,
      node = node,
      shape = shape,
      shapeMap = shapeMap,
      focus = focus,
      resultShapeMap = resultShapeMap
    )

  private def result: RDFParser[Result] =
    for {
      n <- getNode
      v <- n match {
        case BooleanLiteral(b) => parseOk(BooleanResult(b))
        case iri: IRI =>
          for {
            b <- noType
            v <- if (b) {
              val r: RDFParser[Result] = ok(IRIResult(iri))
              r
            } else compoundResult
          } yield v
        case bNode: BNode => compoundResult
        case _            => parseFail("Unexpected type of result " + n)
      }
    } yield v

  private def compoundResult: RDFParser[Result] =
    for {
      n         <- getNode
      maybeType <- optional(iriFromPredicate(rdf_type))
      v <- maybeType match {
        case None => parseFail(s"compoundResult. No rdf:type for node: $n")
        case Some(`sht_ResultShapeMap`) =>
          for {
            iri <- iriFromPredicate(`sht_resultShapeMap`)
          } yield ResultShapeMapIRI(iri)
        case Some(other) => parseFail(s"Unsupported type of compound result: $other")
      }
    } yield v

  private def noType: RDFParser[Boolean] =
    for {
      types <- objectsFromPredicate(rdf_type)
    } yield types.isEmpty

  private def includes: ManifestParser[List[(IRI, Option[ShExManifest])]] = for {
    ctx <- getContext 
    v <- if (ctx.derefIncludes) {
      for {
        is <- liftParser(irisFromPredicate(mf_include))
        result <- {
          val ds: List[ManifestParser[(IRI, Option[ShExManifest])]] =
            is.toList.map(iri => local(ctx => ctx.copy(visited = iri +: ctx.visited), derefInclude(iri, ctx.base, ctx.derefIncludes)))
          ds.sequence
        }
      } yield result
    } else liftParser(parseOk(List()))
  } yield v

  private def derefInclude(node: IRI, base: Option[IRI], derefIncludes: Boolean): ManifestParser[(IRI, Option[ShExManifest])] = node match {
    case iri: IRI => if (derefIncludes) {
        val iriResolved = base.fold(iri)(base => base.resolve(iri))
        for {
          rdf <- liftParser(derefRDF(iriResolved))
          mfs <- parseManifest(iriResolved, rdf) 
          manifest <- liftParser(checkListManifests(mfs))
        } yield (iri, Some(manifest))
      } else liftParser(ok((iri, None)))
    case _ =>
      liftParser(parseFail(s"Trying to deref an include from node $node which is not an IRI"))
  }

  private def parseManifest(iri: IRI, rdf: RDFReader): ManifestParser[List[ShExManifest]] = for {
    ctx <- getContext
    mfs <- if (ctx.visited contains iri) liftParser(parseFail(s"Loop including iri: $iri"))
           else {
             val newCtx = ManifestContext(base = Some(iri), ctx.derefIncludes, ctx.visited)
             liftParser(withRdf(rdf, rdf2Manifest.run(newCtx)))
           } 
  } yield mfs

  private def checkListManifests(mfs: List[ShExManifest]): RDFParser[ShExManifest] = 
    if (mfs.size == 1) ok(mfs.head)
    else parseFail(s"More than one manifests found: ${mfs} at iri $iri")

  private def derefRDF(iri: IRI): RDFParser[RDFReader] = 
    fromEither(RDFAsJenaModel.fromURI(iri.getLexicalForm, "TURTLE", Some(iri)))

  private def parsePropertyList[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] =
    for {
      ls <- rdfListForPredicateAllowingNone(pred)
      vs <- parseNodes(ls, parser)
    } yield vs

  private def mapOptional[A, B](optA: Option[A], fn: A => Either[String, B]): RDFParser[Option[B]] = {
    optA match {
      case None => ok(None)
      case Some(x) => {
        fromEither(fn(x).map(_.some))
      }
    }
  }

  def oneOfPredicates(predicates: Seq[IRI]): RDFParser[IRI] = {
    val ps = predicates.map(iriFromPredicate(_))
    oneOf(ps)
  }

  /**
    * Override this method to provide more info
    */
  /* override def objectFromPredicate(p: IRI): RDFParser[RDFNode] =
    for {
      rdf <- getRDF
      n   <- getNode
      ts  <- fromEither(rdf.triplesWithSubjectPredicate(n, p))
      r <- ts.size match {
        case 0 =>
          parseFail(
            s"objectFromPredicate: Not found triples with subject $n and predicate $p \nRDF: ${rdf.serialize("TURTLE")}"
          )
        case 1 => parseOk(ts.head.obj)
        case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
      }
    } yield r
 */
}

object RDF2Manifest extends LazyLogging {

/*  def read(
      fileName: String,
      format: String,
      base: Option[String],
      derefIncludes: Boolean
  ): Either[String, ShExManifest] = {
    val noIri : Option[IRI] = None
    for {
      cs  <- getContents(fileName)
      rdf <- RDFAsJenaModel.fromChars(cs, format, base.map(IRI(_)))
      iriBase <- base match {
        case None      => { 
          Right(noIri)
        }
        case Some(str) => { 
          IRI.fromString(str).map(Some(_))
        }
      }
      mfs <- {
        val ctx = RDF2Manifest.Context()
        RDF2Manifest(iriBase, derefIncludes).rdf2Manifest(rdf).run(ctx)
      }
      manifest <- if (mfs.size == 1) { 
        Right(mfs.head)
      } 
      else Left(s"Number of manifests != 1: ${mfs}")
    } yield manifest
  }
 */
  def read(
      fileName: String,
      format: String,
      base: Option[String],
      derefIncludes: Boolean
  ): EitherT[IO, String, ShExManifest] = {
    val noIri : Option[IRI] = None
    val n : RDFNode = IRI("http://internal.base/")
    for {
      cs  <- getContentsIO(fileName)
      rdf <- getRDF(cs, format, base)
      iriBase <- base match {
        case None      => { 
          val s: EitherT[IO, String,Option[IRI]] = EitherT.pure(noIri) 
          s
        }
        case Some(str) => { 
          val s: Either[String,Option[IRI]] = IRI.fromString(str).map(Some(_))
          val r: EitherT[IO, String, Option[IRI]] = EitherT.fromEither(s) 
          r
        }
      }
      mfs <- {
        val ctx = ManifestContext(iriBase,derefIncludes,List())
        val cfg = Config(n,rdf)
        EitherT((new RDF2Manifest).rdf2Manifest.run(ctx).value.run(cfg))
      }
      manifest <- if (mfs.size == 1) { 
        val r: EitherT[IO,String,ShExManifest] = EitherT.pure(mfs.head)
        r
      } 
        else EitherT.leftT[IO,ShExManifest](s"Number of manifests != 1: ${mfs}")
    } yield manifest
  }

//  private def ok[A](x:A): EitherT[IO, String, A] = EitherT.pure(x)
//  private def err[A](s: String): EitherT[IO,String,A] = EitherT.left[A](s)

  private def getRDF(cs: String, format: String, base: Option[String]): EitherT[IO, String, RDFReader] = {
    val s = RDFAsJenaModel.fromChars(cs, format, base.map(IRI(_)))
    EitherT.fromEither(s)
  }

}
