package es.weso.rdf.jena

import cats.effect._
import cats.implicits._
import es.weso.utils.IOUtils._
import es.weso.rdf.nodes._
import org.scalatest._
import org.scalatest.matchers.should._
import org.scalatest.funspec._
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query._
import org.apache.jena.system.Txn

import scala.util.Try

// TODO: Refactor the following code to use IO Resources (bracket) to ensure that the resources are closed...

class EndpointTest
  extends AnyFunSpec
  with JenaBased
  with Matchers with BeforeAndAfter {

  val dataset = "ds"
  val portNumber: Int = 5678
  val endpointName = s"http://localhost:${portNumber}/${dataset}/sparql"
  val ex = IRI("http://example.org/")

  val acquireServer: IO[(FusekiServer,Dataset)] = IO {
    val ds = DatasetFactory.createTxnMem
    val server = FusekiServer.create.port(portNumber).add(s"/${dataset}", ds).build
    server.start()
    (server,ds)
  }
  def releaseServer(pair: (FusekiServer,Dataset)): IO[Unit] = {
    val (server,ds) = pair
    IO {
      ds.close()
      server.stop()
    }
  }
  val server: IO[Resource[IO,(FusekiServer, Dataset)]] = IO(Resource.make(acquireServer)(releaseServer))

  val endpoint = Endpoint(IRI(endpointName))
  val endpointUpdate = s"http://host:3330/${dataset}/update"

  describe("Checking endpoint") {


    it("should be able to obtain SHACL instances in an endpoint") {
      shouldObtainShaclInstances(
        s"""|prefix : ${ex}
            |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            |:x a :A .
            |:y a :B .
            |:A rdfs:subClassOf :B .
            """.stripMargin,
        ex + "B",
        Seq(ex+"x",ex+"y")
      )
    }

    it("should be able to obtain SHACL instances in an endpoint 2") {
      shouldObtainShaclInstances(
        s"""|prefix : ${ex}
            |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            |:x a :A .
            |:y a :B .
            |:A rdfs:subClassOf :B .
            """.stripMargin,
        ex + "B",
        Seq(ex+"x",ex+"y")
      )
    }
    it("should be able to run a query over an endpoint") {
      shouldQueryData(
        s"""|prefix : ${ex}
            |:a :p 2,3 .
            """.stripMargin,
        s"""|prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |prefix : $ex
           |select * {
           |?x ?p ?y
           |}
           |""".stripMargin,
        List(Map("x" -> (ex + "a"), "p" -> (ex + "p"), "y" -> IntegerLiteral(3)),
             Map("x" -> (ex + "a"), "p" -> (ex + "p"), "y" -> IntegerLiteral(2)))
      )
    } 

    def withEndpoint[A](data: String,
                        action: Endpoint => IO[A]
                       ): IO[A] = for {
                         resServer <- server
                         resRdf <- RDFAsJenaModel.fromChars(data, "Turtle")
                         v <- (resServer,resRdf).tupled.use { 
                           case (server,rdf) => for {
                             model <- rdf.getModel
                             result <- {
          val (_,ds) = server
          Txn.executeWrite(ds, execute(ds.setDefaultModel(model)))
          action(endpoint)
        }
      } yield result 
    } } yield v

    def shouldObtainShaclInstances(data: String, node: IRI, expected: Seq[RDFNode]): Unit = {
      val either = io2ES(withEndpoint(data, _.getSHACLInstances(node).compile.toList))
      info(s"shouldObtainShaclInstances for node $node")
      either.fold(e => fail(s"Error: $e"), is => {
        is should contain theSameElementsAs(expected)
      })
    }

    def shouldQueryData(data: String, queryStr: String, expected: List[Map[String, RDFNode]]): Unit = {
      val r: IO[List[Map[String,RDFNode]]] = 
       for {
        rs <- server 
        resRdf <- RDFAsJenaModel.fromChars(data,"Turtle")
        v <- (rs,resRdf).tupled.use{ case (pair,rdf) => 
          for {
           model <- rdf.getModel
           rm <- Try {
            val (_,ds) = pair
             Txn.executeWrite(ds, execute(ds.setDefaultModel(model)))
             val query = QueryFactory.create(queryStr)
             val rs = QueryExecutionFactory.sparqlService(endpointName, query).execSelect()
          JenaMapper.resultSet2Map(rs)
          }.fold(
          e => IO.raiseError(e),
          maybeRM => maybeRM.fold(e => IO.raiseError(new RuntimeException(e)), rm => IO(rm))
        )
      } yield rm}
    } yield v
      r.attempt.unsafeRunSync().fold(
        e => fail(s"Error: $e"),
        rm => shouldCompareListMaps(rm, expected))
    }

    def execute(body: => Unit): Runnable = {
      () => body
    }

    def shouldCompareListMaps(m1: List[Map[String,RDFNode]], m2: List[Map[String,RDFNode]]):Unit = {
      m1.zip(m2).foreach(pair => {
        shouldCompareMaps(pair._1,pair._2)
      })
    }

    def shouldCompareMaps(m1: Map[String,RDFNode], m2: Map[String,RDFNode]):Unit = {
      for (key <- m1.keys) {
        val e = m2(key).isEqualTo(m1(key))
        e.fold(s => fail(s"Error comparing values for key: $key. ${m1(key)} vs ${m2(key)}\nError:$s\nMap1: $m1\n$m2"), b => {
          if (!b) {
            fail(s"Different value for $key. ${m1(key)}!=${m2(key)}\nMap1: $m1\nMap2: $m2")
          }
        })
      }
    }
  }

}
