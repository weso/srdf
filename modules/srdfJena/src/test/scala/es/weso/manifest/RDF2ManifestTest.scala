package es.weso.shextest.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec

class RDF2ManifestTest extends AnyFunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("localFolderTest")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
    parseManifest("manifest","",shexFolder)
  }
}
