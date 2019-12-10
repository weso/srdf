package es.weso.shextest.manifest

import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
// import es.weso.shapeMaps.ShapeMap
import es.weso.utils.FileUtils
import cats.data.EitherT
import cats.effect.IO
import org.scalatest._

import scala.util.{Either, Left, Right, Try}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait ValidateManifest extends AnyFunSpec with Matchers with TryValues with OptionValues {

  def parseManifest(name: String,folder: String, parentFolder: String): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      println(s"ParseManifest: $name, folder: $folder, parentFolder: $parentFolder")
      val parentFolderURI = Try {Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
      println(s"ParentFolderUri: $parentFolderURI")
      val manifestFolder = s"$parentFolder/$folder"
      val fileName = s"$manifestFolder/$name.ttl"
      RDF2Manifest.read(fileName, "TURTLE", Some(s"$parentFolderURI/$folder/"), true).value.unsafeRunSync match {
        case Left(e) =>
          fail(s"Error reading $fileName\nError: $e")
        case Right(mf) => {
          processManifest(mf,name,manifestFolder)
        }
      }
    }
  }

  def processManifest(m: ShExManifest, name: String, parentFolder: String): Unit = {
    println(s"processManifest with ${name} and parent folder $parentFolder")
    for ((includeNode, manifest) <- m.includes) {
      println(s"Include: $includeNode")
      val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
      // println(s"Include folder: parent: ${folder.getParent.toString}, fileName: ${folder.getFileName.toString}")
      parseManifest(includeNode.getLexicalForm, folder, parentFolder)
    }
    for (e <- m.entries) {
      processEntry(e, name, parentFolder)
    }
  }

  def processEntry(e: es.weso.shextest.manifest.Entry, name: String, manifestFolder: String): Unit = e match {
    case r: RepresentationTest => {
      println(s"Entry: ${e}, name: $name")
      ()
    }
    case v: Validate => {
      val a = v.action
      val r = for {
            strRdf            <- getContents("data", manifestFolder, a.data)
            strSchema         <- getContents("schema", manifestFolder, a.schema)
            strShapeMap       <- getContents("shapeMap", manifestFolder, a.shapeMap)
            strResultShapeMap <- getContents("resultShapeMap", manifestFolder, a.resultShapeMap)
          } yield {
            (strRdf, strSchema, strShapeMap, strResultShapeMap)
       }
      r.fold(
            e => info(s"Error: $e"),
            v => {
              val (strRdf, strSchema, strShapeMap, strResultShapeMap) = v
              shouldValidateWithShapeMap(strRdf, strSchema, strShapeMap, strResultShapeMap)
            }
          )
       }
    case _ => fail(s"Unsupported entry type: ${e.entryType}")
  }

  def getContents(name: String, folder: String, value: Option[IRI]): EitherT[IO, String, String] = 
  value match {
    case None => EitherT.leftT[IO,String](s"No value for $name")
    case Some(iri) => FileUtils.getContents(folder + "/" + iri.str).map(_.toString)
  }

  def shouldValidateWithShapeMap(
                                  rdfStr: String,
                                  shexStr: String,
                                  shapeMapStr: String,
                                  expected: String): Unit = {
    it(s"Should validate ${shexStr} with ${rdfStr} and ${shapeMapStr} and result $expected") {
      val validate = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle")
      } yield true
      validate match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}