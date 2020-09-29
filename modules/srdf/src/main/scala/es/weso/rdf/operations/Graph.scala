package es.weso.rdf.operations
import es.weso.rdf.nodes._
import es.weso.rdf._
import cats.effect._
import fs2.{Pipe, Stream}
import es.weso.rdf.triples.RDFTriple

object Graph {

  /**
  * traverse the outgoing nodes of a node
    * It currently only takes into account the ourgoing arcs
    * @param node starting node
    * @param rdf RDFReader
    * @return list of visited nodes
    */
  def traverse(node: RDFNode, rdf: RDFReader): Stream[IO,RDFNode] = {

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
        onv <- Stream.eval(outgoingNotVisited(head, visited).compile.toList)
        rs <- loop(onv ++ tail, head :: visited)
      } yield rs
    }
    loop(List(node),List())
  }

  def getTriples(rdf: RDFReader): Pipe[IO, RDFNode, RDFTriple] =
    nodes => for {
     node <- nodes
     triple <- rdf.triplesWithSubject(node)
    } yield triple

  def traverseWithArcs(node: RDFNode, rdf: RDFReader): Stream[IO,RDFTriple] =
    traverse(node,rdf).through(getTriples(rdf))

}