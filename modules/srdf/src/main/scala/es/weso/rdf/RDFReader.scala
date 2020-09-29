package es.weso.rdf
import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import cats.implicits._
import cats.effect._
import fs2.Stream
import es.weso.utils.internal.CollectionCompat._


/**
 * RDFReader can read RDF data from several sources like an in-memory model or a SPARQL endpoint
 */

trait RDFReader {

  type Rdf <: RDFReader
  type RDFRead[A] = IO[A]
  type RDFStream[A] = Stream[IO,A]

  val id: String

  /**
    * @return List of available formats that this RDFReader supports
    */
  def availableParseFormats: List[String]

  /**
    * 
    * @return List of formats in which this RDFReader can be serialized 
    */
  def availableSerializeFormats: List[String]

  /**
    * Parse a char sequence to obtain an RDFReader
    * @param cs char sequence to parse
    * @param format format (TURTLE by default)
    * @param base base IRI (None by default)
    * @return Right RDF or Left error message
    */
  /*def fromString(cs: CharSequence,
                 format: String = "TURTLE",
                 base: Option[IRI] = None): Resource[RDFRead,Rdf] */

  /**
   * convert a RDF graph to a String
   */
  def serialize(format: String = "TURTLE", base: Option[IRI] = None): RDFRead[String]

  /**
   * Set of RDFTriples in a graph
   */
  def rdfTriples(): RDFStream[RDFTriple]

  /**
   * Returns the set of subjects that are IRIs in a graph
   */
  def subjects(): RDFStream[RDFNode] = {
    rdfTriples.map(_.subj)
  }

  /**
   * Returns the set of predicates
   */
  def predicates(): RDFStream[IRI] = {
    rdfTriples.map(_.pred)
  }

  /**
   * Returns the set of iriObjects that are IRIs in a graph
   */
  // TODO: Extend this to return all iriObjects: Seq[RDFNode]
  def iriObjects(): RDFStream[IRI] = {
    rdfTriples.map(_.obj).collect { case i: IRI => i }
  }

  /**
   * The set of all iri's available
   */
  def iris(): RDFStream[IRI] = {
    def f(t: RDFTriple): Stream[IO,IRI] = Stream.emits(t.iris.toSeq)
    rdfTriples.map(f).flatten
  }

  /**
   * Set of RDFTriples that contain a node as subject
   * @param n node
   * @return A set of triples or a String with an error message
   */
  def triplesWithSubject(n: RDFNode): RDFStream[RDFTriple]

  /**
   * Set of RDFTriples that relate two nodes by a predicate
   * @param p predicate
   */
  def triplesWithPredicate(p: IRI): RDFStream[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as object
   * @param n node
   */
  def triplesWithObject(n: RDFNode): RDFStream[RDFTriple]


  /**
   * Set of RDFTriples that contain a node as predicate with some object
   * @param p predicate
   * @param o object
   */
  def triplesWithPredicateObject(p: IRI, o: RDFNode): RDFStream[RDFTriple]

//  def triplesWithPredicateObjectIO(p: IRI, o: RDFNode): RDFRead[Set[RDFTriple]]

  /**
   * Set of RDFTriples that relate two nodes by a SHACL path
   * @param p path
   */
  def nodesWithPath(p: SHACLPath): RDFStream[(RDFNode, RDFNode)]

  /**
   * Set of RDFTriples that relate a node with some object by a path
   * @param p path
   * @param o object
   */
  def subjectsWithPath(p: SHACLPath, o: RDFNode): RDFStream[RDFNode]

  /**
   * return the values associated with a node by a path
   * The path is defined as in SHACL paths which are a simplified version of SPARQL paths
   */
  def objectsWithPath(subj: RDFNode, path: SHACLPath): RDFStream[RDFNode]

  def triplesWithType(expectedType: IRI): RDFStream[RDFTriple] = {
    triplesWithPredicateObject(`rdf:type`, expectedType)
  }

  /**
   * Set of RDFTriples that contain a node as subject and a given Predicate
   * @param s
   */
  def triplesWithSubjectPredicate(s: RDFNode, p: IRI): RDFStream[RDFTriple] = 
    triplesWithSubject(s).filter(_.hasPredicate(p))

  def hasPredicateWithSubject(n: RDFNode, p: IRI): RDFRead[Boolean] 

  def mkSeq[A,B,F[_]:Effect](vs: List[A], f: A => Stream[F,B]): Stream[F,B] = {
    vs.traverse(f).map(Stream.emits(_)).flatten
  }
  /*for {
    bs <- vs.map(f).sequence
  } yield bs */

  /**
    * Set of RDFTriples that contain a node as object with some of the predicates in a list
    * @param o object
    * @param ps list of predicates
    */
  def triplesWithPredicatesObject(ps: LazyList[IRI], o: RDFNode): RDFStream[RDFTriple] = {
    mkSeq(ps.toList, (p: IRI) => triplesWithPredicateObject(p,o))
  }

  /**
    * Set of RDFTriples that contain a node as subject with some of the predicates in a list
    * @param n node
    * @param ps list of predicates
    */
  def triplesWithSubjectPredicates(n: RDFNode, ps: LazyList[IRI]): RDFStream[RDFTriple] = {
    mkSeq(ps.toList, (p: IRI) => triplesWithSubjectPredicate(n,p))
  }

  /**
   * Prefix map
   */
  def getPrefixMap: RDFRead[PrefixMap]

  /**
   * `true` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def hasSHACLClass(node: RDFNode, cls: RDFNode): RDFRead[Boolean]

  /**
   * return the SHACL instances of a node `cls`
   * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def getSHACLInstances(cls: RDFNode): RDFStream[RDFNode]

  def getTypes(node: RDFNode): RDFStream[RDFNode] = {
    triplesWithSubjectPredicate(node, `rdf:type`).map(_.obj)
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
  def querySelect(queryStr: String): RDFStream[Map[String,RDFNode]]

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

  def subjectsWithType(t: RDFNode): RDFStream[RDFNode] = {
    triplesWithPredicateObject(`rdf:type`, t).map(_.subj)
  }

  def subjectsWithProperty(pred: IRI): RDFStream[RDFNode] = {
    triplesWithPredicate(pred).map(_.subj)
  }

}

