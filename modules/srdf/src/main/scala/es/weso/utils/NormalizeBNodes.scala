package es.weso.utils

import cats._
import cats.data._
import cats.syntax.all._
import cats.effect._
import es.weso.rdf.nodes.{BNode, IRI, Literal, RDFNode}
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.{RDFBuilder, RDFReader}

// TODO: This code could be deprecated
object NormalizeBNodes {

  def normalizeBNodes(rdf: RDFReader, target: RDFBuilder): IO[RDFBuilder] = {

    type BNodeMap = Map[String,String]

    type Cnv[A] = StateT[Id,BNodeMap,A]

    def ok[A](x: A): Cnv[A] = StateT.pure(x)
    // def cnvBNode(b: BNode): Cnv[BNode] = ok(b)

    def cnvNode(n: RDFNode): Cnv[RDFNode] = n match {
      case i: IRI => ok(i)
      case l: Literal => ok(l)
      case b: BNode => for {
        bnodeMap <- StateT.get[Id,BNodeMap]
        bn <- bnodeMap.get(b.id) match {
          case None => {
            val newId = bnodeMap.size.toString
            for {
              _ <- StateT.set[Id, BNodeMap](bnodeMap.updated(b.id, newId))
            } yield BNode(newId)
          }
          case Some(id) => ok(BNode(id))
        }
      } yield bn
    }

    def cnvTriple(t: RDFTriple): Cnv[RDFTriple] = for {
      s <- cnvNode(t.subj)
      o <- cnvNode(t.obj)
    } yield RDFTriple(s,t.pred,o)

/*    def sequence[A](ls: List[Cnv[A]]): Cnv[List[A]] = {
      ls.sequence[Cnv,A]
    } */

    // TODO: Not sure if it works with bNodes
    def cmpTriples(t1: RDFTriple, t2: RDFTriple): Boolean = t1.toString < t2.toString

    for {
      triples <- rdf.rdfTriples.compile.toList
      orderedTriples = triples.sortWith(cmpTriples)
      newTriples = orderedTriples.map(cnvTriple(_)).sequence
      pair = newTriples.run(Map[String,String]())
      (_,ts) = pair
      result <- target.addTriples(ts.toSet)
    } yield result
  }


}