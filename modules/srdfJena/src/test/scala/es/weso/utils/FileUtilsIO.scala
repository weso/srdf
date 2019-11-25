package es.weso.utils
import cats.data.EitherT
import cats.implicits._
import cats.effect._
import scala.io._
import java.io._
import es.weso.utils.FileUtils

object FileUtilsIO {

 def getContentsIO(path: String): EitherT[IO, String, String] = 
   EitherT (IO { FileUtils.getContents(path).map(_.toString) })
 /* EitherT.fromEither (
    try {
        Source.fromResource(path)(Codec.UTF8).getLines.mkString.asRight[String]
    } catch {
      case e: FileNotFoundException =>
        Left(s"Error reading file ${path}: ${e.getMessage}")
      case e: IOException =>
        Left(s"IO Exception reading file ${path}: ${e.getMessage}")
      case e: Exception =>
        Left(s"Exception reading file ${path}: ${e.getMessage}")
    }
  )
 */
}