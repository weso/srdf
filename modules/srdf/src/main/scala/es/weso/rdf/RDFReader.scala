package es.weso.rdf
import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import cats.data._
import cats.implicits._
import cats.data.EitherT
import cats.effect._
import fs2.Stream

/**
 * RDFReader can read RDF data from several sources like an in-memory model or a SPARQL endpoint
 */

trait RDFReader {

  type Rdf <: RDFReader
  type ES[A] = Either[String, A]  // This is only needed to keep IntelliJ happy
  type RDFRead[A] = EitherT[IO, Throwable, A]
  type RDFReadStream[A] = EitherT[IO, Throwable, Stream[IO,A]]
  

  val id: String

  def fromES[A](e: Either[String,A]): RDFRead[A] = EitherT.fromEither(e.leftMap(RDFException.fromString(_)))

  def err[A](msg: String): RDFRead[A] = EitherT.leftT[IO,A](RDFException.fromString(msg))

  def streamFromLazyList[A](ls: LazyList[A]): Stream[IO,A] = Stream.emits(ls)

  def availableParseFormats: List[String]

  def availableSerializeFormats: List[String]

  /**
    * Parse a char sequence to obtain an RDFReader
    * @param cs char sequence to parse
    * @param format format (TURTLE by default)
    * @param base base IRI (None by default)
    * @return Right RDF or Left error message
    */
  def fromString(cs: CharSequence,
                 format: String = "TURTLE",
                 base: Option[IRI] = None): RDFRead[Rdf]

  /**
   * convert a RDF graph to a String
   */
  def serialize(format: String = "TURTLE", base: Option[IRI] = None): RDFRead[String]

  /**
   * Set of RDFTriples in a graph
   */
  def rdfTriples(): RDFReadStream[RDFTriple]

  /**
   * Returns the set of subjects that are IRIs in a graph
   */
  def subjects(): RDFReadStream[RDFNode] = {
    rdfTriples.map(_.map(_.subj))
  }

  /**
   * Returns the set of predicates
   */
  def predicates(): RDFReadStream[IRI] = {
    rdfTriples.map(_.map(_.pred))
  }

  /**
   * Returns the set of iriObjects that are IRIs in a graph
   */
  // TODO: Extend this to return all iriObjects: Seq[RDFNode]
  def iriObjects(): RDFReadStream[IRI] = {
    rdfTriples.map(_.map(_.obj).collect { case i: IRI => i })
  }

  /**
   * The set of all iri's available
   */
  def iris(): RDFReadStream[IRI] = {
    def f(ts: Stream[IO,RDFTriple]): Stream[IO,IRI] = 
      ts.flatMap(t => Stream.emits(t.iris.toSeq))
    rdfTriples.map(f)
  }

  /**
   * Set of RDFTriples that contain a node as subject
   * @param n node
   * @return A set of triples or a String with an error message
   */
  def triplesWithSubject(n: RDFNode): RDFReadStream[RDFTriple]

  /**
   * Set of RDFTriples that relate two nodes by a predicate
   * @param p predicate
   */
  def triplesWithPredicate(p: IRI): RDFReadStream[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as object
   * @param n node
   */
  def triplesWithObject(n: RDFNode): RDFReadStream[RDFTriple]


  /**
   * Set of RDFTriples that contain a node as predicate with some object
   * @param p predicate
   * @param o object
   */
  def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFReadStream[RDFTriple]

//  def triplesWithPredicateObjectIO(p: IRI, o: RDFNode): RDFRead[Set[RDFTriple]]

  /**
   * Set of RDFTriples that relate two nodes by a SHACL path
   * @param p path
   */
  def nodesWithPath(p: SHACLPath): RDFReadStream[(RDFNode, RDFNode)]

  /**
   * Set of RDFTriples that relate a node with some object by a path
   * @param p path
   * @param o object
   */
  def subjectsWithPath(p: SHACLPath, o: RDFNode): RDFReadStream[RDFNode]

  /**
   * return the values associated with a node by a path
   * The path is defined as in SHACL paths which are a simplified version of SPARQL paths
   */
  def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFReadStream[RDFNode]

  def triplesWithType(expectedType: IRI): RDFReadStream[RDFTriple] = {
    triplesWithPredicateObject(`rdf:type`, expectedType)
  }

  /**
   * Set of RDFTriples that contain a node as subject and a given Predicate
   * @param s
   */
  def triplesWithSubjectPredicate(s: RDFNode, p: IRI): RDFReadStream[RDFTriple] = for {
    // This is the default implementation which is not optimized
    // Implementations of RDFReader could override this implementation by a more efficient one
    ts <- triplesWithSubject(s)
  } yield ts.filter(_.hasPredicate(p))

  def hasPredicateWithSubject(n: RDFNode, p: IRI): RDFRead[Boolean] = for {
     tss <- triplesWithSubjectPredicate(n, p) 
     ts <- EitherT.liftF(tss.head.compile.toList)
  } yield !ts.isEmpty

  def mkSeq[A,B,F[_]:Effect,E](vs: List[A], 
  f: A => EitherT[F,E,Stream[F,B]]
       ): EitherT[F,E,Stream[F,B]] = {
    vs.traverse(f).map(s => Stream.emits(s).flatten) 
  }
  /*for {
    bs <- vs.map(f).sequence
  } yield bs */

  /**
    * Set of RDFTriples that contain a node as object with some of the predicates in a list
    * @param o object
    * @param ps list of predicates
    */
  def triplesWithPredicatesObject(ps: LazyList[IRI], o: RDFNode): RDFReadStream[RDFTriple] = {
    mkSeq(ps.toList, (p: IRI) => triplesWithPredicateObject(p,o))
  }

  /**
    * Set of RDFTriples that contain a node as subject with some of the predicates in a list
    * @param n node
    * @param ps list of predicates
    */
  def triplesWithSubjectPredicates(n: RDFNode, ps: LazyList[IRI]): RDFReadStream[RDFTriple] = {
    mkSeq(ps.toList, (p: IRI) => triplesWithSubjectPredicate(n,p))
  }

  /**
   * Prefix map
   */
  def getPrefixMap(): PrefixMap

  /**
   * `true` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def hasSHACLClass(node: RDFNode, cls: RDFNode): RDFRead[Boolean]

  /**
   * return the SHACL instances of a node `cls`
   * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def getSHACLInstances(cls: RDFNode): RDFReadStream[RDFNode]

  def getTypes(node: RDFNode): RDFReadStream[RDFNode] = {
    triplesWithSubjectPredicate(node, `rdf:type`).map(_.map(_.obj))
  }

  /**
    * Checks if a node has a given datatype
    * @param node RDF node to check
    * @param datatype Datatype IRI to check
    * @return In case of a bad formed literal, a Left with a message, otherwise the check
    */
  def checkDatatype(node: RDFNode, datatype: IRI): RDFRead[Boolean]

  /**
    * Run a SPARQL query which returns a JSON representation of the result
    * @param str string representing the SPARQL query
    * @return JSON representation of the result
    */
  def queryAsJson(str: String): RDFRead[Json]

  /**
    * Run a SPARQL select query which returns a result map
    * @param queryStr string representing the SPARQL query
    * @return Either a List of mappings or an error message
    */
  def querySelect(queryStr: String): RDFRead[List[Map[String,RDFNode]]]

  def getNumberOfStatements(): RDFRead[Int]

  /**
  *
    * @param other RDF reader
    * @return true if this RDF graph is isomorphic with other
    */
  def isIsomorphicWith(other: RDFReader): RDFRead[Boolean]

  /**
    * @return Source IRI of this RDF graph if exists
    */
  def sourceIRI: Option[IRI]

  def asRDFBuilder: RDFRead[RDFBuilder]

  def rdfReaderName: String

  def subjectsWithType(t: RDFNode): RDFReadStream[RDFNode] = {
    triplesWithPredicateObject(`rdf:type`, t).map(_.map(_.subj))
  }

/*  def subjectsWithTypeIO(t: RDFNode): RDFReadStream[RDFNode]] = {
    triplesWithPredicateObjectIO(`rdf:type`, t).map(_.map(_.subj))
  } */


  def subjectsWithProperty(pred: IRI): RDFReadStream[RDFNode] = {
    triplesWithPredicate(pred).map(_.map(_.subj))
  }

}

