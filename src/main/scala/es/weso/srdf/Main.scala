package es.weso.srdf

package es.weso.srdf

import cats.implicits._
import com.monovore.decline._
import cats.effect.ExitCode
import java.nio.file.Path
import com.monovore.decline.effect.CommandIOApp
import cats.effect.IO
import _root_.es.weso.rdf.jena.RDFAsJenaModel

case class ShowRDF(rdf: Path, inputFormat: String, outputFormat: String)
object SRDFMain extends CommandIOApp(
  name = "hello-world",
  header = "Says hello!") 
  {
    val rdfPath = Opts.option[Path]("rdf", "Path to the rdf file.")
    val format:Opts[String] = Opts.option[String]("inFormat", help = "Input format").withDefault("TURTLE")
    val outFormat = Opts.option[String]("outFormat", help = "Output format").withDefault("TURTLE")
    val rdf = (rdfPath,format,outFormat).mapN { (path,inputFormat, outputFormat) => ShowRDF(path,inputFormat,outputFormat) }

    override def main: Opts[IO[ExitCode]] = rdf.map { 
        case ShowRDF(path, inFormat,outFormat) => RDFAsJenaModel.fromFile(path.toFile(), inFormat, None).flatMap(_.use(
            rdf => for {
              str <- rdf.serialize(outFormat)
              _ <- IO.println(str)
            } yield ExitCode.Success)
        )
    }
  }
  

