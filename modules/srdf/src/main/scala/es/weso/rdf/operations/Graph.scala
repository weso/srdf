package es.weso.rdf.operations
import es.weso.rdf.nodes._
import es.weso.rdf._
import cats.implicits._
import cats.data.EitherT
import cats.effect._
import es.weso.rdf.triples.RDFTriple

object Graph {

  /**
  * traverse the outgoing nodes of a node
    * It currently only takes into account the ourgoing arcs
    * @param node starting node
    * @param rdf RDFReader
    * @return list of visited nodes
    */
  def traverse(node: RDFNode, rdf: RDFReader): EitherT[IO, Throwable,List[RDFNode]] = {
    // println(s"Traversing from $node\nRDF:\n${rdf.serialize("TURTLE").getOrElse("")}")

    def outgoing(node: RDFNode): EitherT[IO, Throwable, List[RDFNode]] = for {
     ts <- rdf.triplesWithSubject(node)
    } yield ts.toList.map(_.obj)

    def outgoingNotVisited(node: RDFNode,
                           visited: List[RDFNode]
                          ): EitherT[IO, Throwable, List[RDFNode]] = for {
      os <- outgoing(node)
    } yield os.filter(x => x != node && !visited.contains(x))

    // @annotation.tailrec
    // TODO: Refactor to be tailrec again
    def loop(stack: List[RDFNode],
             visited: List[RDFNode]
            ): EitherT[IO, Throwable,List[RDFNode]] = stack match {
      case Nil => EitherT.pure(visited)
      case head :: tail => for {
        onv <- outgoingNotVisited(head, visited)
        rs <- loop(onv ++ tail, head :: visited)
      } yield rs
    }
    loop(List(node),List())
  }

  type ES[A] = EitherT[IO,Throwable,A]
  def sequenceU[A](xs: List[EitherT[IO, Throwable,A]]): EitherT[IO, Throwable,List[A]] = xs.sequence[ES,A]

  def traverseWithArcs(node: RDFNode, rdf: RDFReader): EitherT[IO, Throwable, (List[RDFNode], List[RDFTriple])] = for {
    nodes <- traverse(node,rdf)
    triples <- sequenceU(nodes.map(rdf.triplesWithSubject(_))).map(_.flatten)
  } yield (nodes, triples)

}