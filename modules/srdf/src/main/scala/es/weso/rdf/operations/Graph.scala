package es.weso.rdf.operations
import es.weso.rdf.nodes._
import es.weso.rdf._
//import cats.implicits._
//import cats.data.EitherT
import cats.effect._
import fs2.Stream
import es.weso.rdf.triples.RDFTriple

object Graph {

  /**
  * traverse the outgoing nodes of a node
    * It currently only takes into account the ourgoing arcs
    * @param node starting node
    * @param rdf RDFReader
    * @return list of visited nodes
    */
  def traverse(node: RDFNode, rdf: RDFReader): Stream[IO,RDFNode] = 
   Stream.raiseError[IO](RDFException.fromString("Not implemented traverse yet"))
  
/*  {
    // println(s"Traversing from $node\nRDF:\n${rdf.serialize("TURTLE").getOrElse("")}")

    def outgoing(node: RDFNode): Stream[IO,RDFNode] = 
     rdf.triplesWithSubject(node).map(_.obj)

    def outgoingNotVisited(node: RDFNode,
                           visited: List[RDFNode]
                          ): Stream[IO,RDFNode] = 
      outgoing(node).filter(x => x != node && !visited.contains(x))

    // @annotation.tailrec
    // TODO: Refactor to be tailrec again
    def loop(stack: List[RDFNode],
             visited: List[RDFNode]
            ): Stream[IO,RDFNode] = stack match {
      case Nil => Stream.emits(visited)
      case head :: tail => for {
        onv <- outgoingNotVisited(head, visited)
        rs <- loop(onv ++ tail, head :: visited)
      } yield rs
    }
    loop(List(node),List())
  } */


  def traverseWithArcs(node: RDFNode, rdf: RDFReader): Stream[IO,(List[RDFNode], List[RDFTriple])] = 
    Stream.raiseError[IO](RDFException.fromString(s"Not implemented traverseWithArcs from $node yet"))
  
/*  for {
    nodes <- traverse(node,rdf)
    triples <- sequenceU(nodes.map(rdf.triplesWithSubject(_))).map(_.flatten)
  } yield (nodes, triples)
*/
}