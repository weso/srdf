package es.weso.utils
import org.http4s._
import org.http4s.client._
import org.http4s.client.middleware._
// import org.http4s.dsl._
// import org.http4s.implicits._
import cats.effect._
import org.http4s.headers._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import cats.effect._
import java.net.URI
import java.net.URL
import java.net.HttpURLConnection
import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader

object DerefUtils {


  def withRedirect[F[_]:Concurrent](c: Client[F]): Client[F] = FollowRedirect(10, _ => true)(c)

  def derefIRI(iri: Uri, client: Client[IO]): IO[String] = {
    lazy val `text/turtle` = new MediaType("text", "turtle")
    val redirectClient = withRedirect(client)
    val req: Request[IO] = Request(method = Method.GET, uri = iri).withHeaders(`Accept`(`text/turtle`))
    // val v: F[String] = redirectClient.expect[String](req)
    redirectClient.expect[String](req)
  }

  def iri2uri(iri: IRI): IO[Uri] = Uri.fromString(iri.str).fold(e => 
    IO.raiseError(new RuntimeException(s"Error converting $iri to Uri: $e")),
    IO.pure(_)
  )

  def derefRDFJava(uri: URI): IO[String] = IO {
    // Untested code...
    val url: URL = uri.toURL
    val connection: HttpURLConnection = url.openConnection().asInstanceOf[HttpURLConnection];
    connection.connect()
    val is: InputStream = connection.getInputStream
    is.toString()   
  }


}
