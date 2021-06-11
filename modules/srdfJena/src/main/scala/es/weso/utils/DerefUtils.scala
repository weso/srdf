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
// import java.net._
import java.net.http._
import java.net.http.HttpResponse.BodyHandlers
import java.time.Duration
import java.net.http.HttpClient.Redirect
import cats.effect._

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

  def derefRDF(iri: IRI, client: Client[IO]): Resource[IO,RDFReader] = {
    /*
    for {
      uri <- iri2uri(iri)
      str <- derefIRI(uri, client)
      rdf <- RDFAsJenaModel.fromString(str,"TURTLE")
    } yield rdf */
    ???
  }

  def derefRDFJava(iri: IRI): IO[RDFReader] = /* for {
    str <- IO {
      val client = HttpClient.newBuilder().followRedirects(Redirect.ALWAYS).build()
      val request: HttpRequest = HttpRequest.newBuilder()
      .uri(iri.uri)
      .timeout(Duration.ofMinutes(4))
      .header("Accept", "text/turtle").GET.build()
      println(s"Request: ${request}")
      val response = client.send(request, BodyHandlers.ofString)
      println(s"Body: ${response.body()}\nEND BODY (drefJava)")
      response.body()
    }
    rdf <- RDFAsJenaModel.fromString(str,"TURTLE")
  } yield rdf */
  ???

}
