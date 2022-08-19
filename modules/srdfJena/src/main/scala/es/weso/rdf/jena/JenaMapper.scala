package es.weso.rdf.jena

// TODO: Refactor this code
import org.apache.jena.rdf.model.{AnonId, ModelFactory, Property, ResourceFactory, Statement, Literal => JenaLiteral, Model => JenaModel, RDFNode => JenaRDFNode, Resource => JenaResource}
import es.weso.rdf.nodes._
import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import es.weso.rdf.triples.RDFTriple

import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import es.weso.rdf.path._
import es.weso.utils.EitherUtils
import org.apache.jena.sparql.path._
import util._
import org.apache.jena.query._
import cats.effect._
import es.weso.utils.IOUtils._
import cats.implicits._
import org.apache.jena.graph.Node
// import es.weso.rdf.RDFException
import es.weso.rdf.MsgRDFException


object JenaMapper {

//   val logger = Logger("JenaMapper")

  lazy val emptyModel = ModelFactory.createDefaultModel()

  def RDFTriples2Model(triples: Set[RDFTriple],
                       m: JenaModel,
                       base: Option[IRI]
                      ): JenaModel = {
    for (t <- triples) {
      val subj = createResource(m, t.subj,base)
      val pred = createProperty(m, t.pred,base)
      val obj = createRDFNode(m, t.obj,base)
      val stmt = m.createStatement(subj, pred, obj)
      m.add(stmt)
    }
    m
  }

  def RDFTriple2Statement(triple: RDFTriple): Statement = {
    // TODO: implement
    throw new Exception("RDFTriple2Statement: unimplemented conversion from " + triple)
  }

  def statement2RDFTriple(s: Statement): IO[RDFTriple] = for {
    subj <- jenaNode2RDFNode(s.getSubject)
    pred = property2IRI(s.getPredicate)
    obj <- jenaNode2RDFNode(s.getObject)
  } yield
    RDFTriple(subj, pred, obj)

  def jenaTriple2Triple(subj: Node, pred: Node, obj: Node): IO[RDFTriple] = for {
    subj <- node2RDFNode(subj)
    pred <- node2IRI(pred)
    obj <- node2RDFNode(obj)
  } yield
    RDFTriple(subj, pred, obj)

  def unsafeJenaTriple2Triple(subj: Node, pred: Node, obj: Node): RDFTriple = {
    val s = unsafeNode2RDFNode(subj)
    val p = unsafeNode2IRI(pred)
    val o = unsafeNode2RDFNode(obj)
    RDFTriple(s, p, o)  
  }

  def node2RDFNode(node: Node): IO[RDFNode] = IO(unsafeNode2RDFNode(node)
  ) 

  def unsafeNode2RDFNode(node: Node): RDFNode = node match {
    case _ if node.isURI() => IRI(node.getURI())
    case _ if node.isLiteral() => DatatypeLiteral(node.getLiteralLexicalForm(), IRI(node.getLiteralDatatypeURI()))
    case _ if node.isBlank() => BNode(node.getBlankNodeLabel())
    case _ => throw new MsgRDFException(s"node2RDFNode: Unknown value: ${node}")
  }

  def node2IRI(node: Node): IO[IRI] = IO(unsafeNode2IRI(node))

  def unsafeNode2IRI(node: Node): IRI = node match {
    case _ if node.isURI() => IRI(node.getURI())
    case _ => throw new MsgRDFException(s"node2IRI: Unknown value: ${node}")
  }


  private def resolve(iri: IRI, base: Option[IRI]): String = base match {
    case None => iri.str
    case Some(baseIri) => baseIri.resolve(iri).str
  }

  def rdfNode2Property(n: RDFNode,
                       m: JenaModel,
                       base: Option[IRI]
                      ): IO[Property] = {
    n match {
      case i: IRI => ok(m.getProperty(resolve(i,base)))
      case _ => err("rdfNode2Property: unexpected node " + n)
    }
  }

  def rdfNode2Resource(n: RDFNode,
                       m: JenaModel,
                       base: Option[IRI]): IO[JenaResource] = {
    n match {
      case i: IRI => ok(m.getResource(resolve(i,base)))
      case b: BNode => {
        // Creates the BNode if it doesn't exist
        ok(m.createResource(new AnonId(b.id)))
      }
      case _ => err(s"rdfNode2Resource: $n is not a resource")
    }
  }

  def model2triples(model: JenaModel): IO[List[RDFTriple]] = {
    model.listStatements.asScala.map(st => statement2triple(st)).toList.sequence
  }

  def statement2triple(st: Statement): IO[RDFTriple] =
    for {
      subj <- jenaNode2RDFNode(st.getSubject)
      obj  <- jenaNode2RDFNode(st.getObject)
    } yield RDFTriple(subj, property2iri(st.getPredicate), obj)

  def property2iri(p: Property): IRI = {
      IRI(p.getURI)
  }

  def rdfNode2JenaNode(n: RDFNode, m: JenaModel, base: Option[IRI]): JenaRDFNode =
    createRDFNode(m, n, base)

/*  def jenaNode2RDFNodeUnsafe(r: JenaRDFNode): RDFNode = {
    jenaNode2RDFNode(r).fold(e => StringLiteral(s"Error: $e"), identity)
  } */

  /**
  * If str is "xsd:year" returns http://www.w3.org/2001/XMLSchema#
    * @param str
    * @return
    */
  private def extendNS(str: String): Option[IRI] = {
    val xsdIRI = IRI("http://www.w3.org/2001/XMLSchema#")
    val xsdRegex = raw"xsd\:([A-Za-z]+)".r
    str match {
      case xsdRegex(name) => Some(xsdIRI.add(name))
      case other => IRI.fromString(other,None).toOption
    }
  }

  def jenaNode2RDFNode(r: JenaRDFNode): IO[RDFNode] = r match {
    case _ if r.isAnon =>  {
      val b = BNode(r.asResource().getId.getLabelString)
      ok(b)
    }
    case _ if r.isURIResource => {
      ok(IRI(r.asResource.getURI))
    }
    case lit: JenaLiteral => {
      extendNS(lit.getDatatype.getURI) match {
        case  Some(RDFNode.`RDFhtmlStringDatatypeIRI`) => // RH20220819
          ok(RDFhtmlStringLiteral(lit.getLexicalForm))
        case None | Some(RDFNode.`StringDatatypeIRI`) =>
          ok(StringLiteral(lit.getLexicalForm))
        case Some(RDFNode.`IntegerDatatypeIRI`) => {
          Try(IntegerLiteral(lit.getLexicalForm.toInt, lit.getLexicalForm)).fold(
            e => ok(DatatypeLiteral(lit.getLexicalForm, RDFNode.IntegerDatatypeIRI)),
            ok(_)
          )
        }
       case Some(RDFNode.`DecimalDatatypeIRI`) =>
              Try(DecimalLiteral(lit.getLexicalForm.toDouble, lit.getLexicalForm)).fold(
                e => ok(DatatypeLiteral(lit.getLexicalForm, RDFNode.DecimalDatatypeIRI)),ok(_))
       case Some(RDFNode.`DoubleDatatypeIRI`) =>
              Try(DoubleLiteral(lit.getLexicalForm.toDouble,lit.getLexicalForm)).fold(
                e => ok(DatatypeLiteral(lit.getLexicalForm, RDFNode.DoubleDatatypeIRI)),ok(_))
       case Some(RDFNode.BooleanDatatypeIRI) => {
              // Lexical form of boolean literals is lowercase true or false
              lit.getLexicalForm match {
                case "true" => ok(BooleanLiteral(true))
                case "false" => ok(BooleanLiteral(false))
                case _ => ok(DatatypeLiteral(lit.getLexicalForm, RDFNode.BooleanDatatypeIRI))
              }
            }
       case Some(RDFNode.`LangStringDatatypeIRI`) => {
              // TODO: Check that the language tag conforms to BCP 47 (https://tools.ietf.org/html/bcp47#section-2.1)
              ok(LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage)))
            }
       case Some(datatype) => {
              ok(DatatypeLiteral(lit.getLexicalForm, datatype))
            }
          }
        }
   case _ =>
    err(s"resource2RDFNode: unexpected type of resource: $r")
  }

  def property2IRI(p: Property): IRI = IRI(p.getURI)
  def iri2Property(iri: IRI): Property = ResourceFactory.createProperty(iri.str)

  def createResource(m: JenaModel, node: RDFNode, base: Option[IRI]): JenaResource = {
    node match {
      case b: BNode => m.createResource(new AnonId(b.id.toString))
      case i: IRI => m.createResource(resolve(i,base))
      case _ => throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createRDFNode(m: JenaModel,
                    node: RDFNode,
                    base: Option[IRI]): JenaRDFNode = {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val xsdinteger = xsd + "integer"
    val xsddouble = xsd + "double"
    val xsddecimal = xsd + "decimal"
    val xsdboolean = xsd + "boolean"

    node match {
      case b: BNode =>
        m.createResource(new AnonId(b.id.toString))
      case i: IRI =>
        m.createResource(resolve(i,base))
      case StringLiteral(str) =>
        m.createLiteral(str, false)
      case DatatypeLiteral(str, i: IRI) =>
        i.str match {
          case `xsdinteger` => m.createTypedLiteral(str, XSDDatatype.XSDinteger)
          case `xsddouble` => m.createTypedLiteral(str, XSDDatatype.XSDdouble)
          case `xsddecimal` => m.createTypedLiteral(str, XSDDatatype.XSDdecimal)
          case `xsdboolean` => m.createTypedLiteral(str, XSDDatatype.XSDboolean)
          case _ => m.createTypedLiteral(str, new BaseDatatype(i.str))
        }
      case l@DecimalLiteral(d, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDdecimal)
      case l@IntegerLiteral(i, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDinteger)
      case LangLiteral(l, Lang(lang)) => m.createLiteral(l, lang)
      case BooleanLiteral(b) =>
        m.createTypedLiteral(b.toString, XSDDatatype.XSDboolean)
      case l@DoubleLiteral(d: Double, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDdouble)
      case _ =>
        throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createProperty(m: JenaModel, pred: IRI, base: Option[IRI]): Property = {
    m.createProperty(resolve(pred,base))
  }

  def triplesSubject(resource: JenaResource, model: JenaModel): IO[Set[Statement]] =
  Try {
    model.listStatements(resource, null, null).toSet.asScala.toSet
  }.fold(e => 
     err(s"triplesSubject: Error obtaining statements from $resource: ${e.getLocalizedMessage()}"
     ), ok(_))

  def triplesSubjectPredicate(resource: JenaResource,
                              pred: IRI,
                              model: JenaModel,
                              base: Option[IRI]
                             ): IO[Set[Statement]] = Try {
    model.listStatements(resource, createProperty(model,pred,base), null).toSet.asScala.toSet
  }.fold(e => 
      err(s"triplesSubjectPredicate: Error obtaining triples from $resource: ${e.getLocalizedMessage()}"
      ), ok(_))

  def triplesPredicateObject(pred: IRI,
                             obj: JenaRDFNode,
                             model: JenaModel,
                             base: Option[IRI]
                            ): IO[Set[Statement]] = {
    IO(model.listStatements(null, createProperty(model,pred,base), obj).toSet.asScala.toSet)
  }

  def triplesPredicate(pred: Property, model: JenaModel): IO[Set[Statement]] =
  Try {
    model.listStatements(null, pred, null).toSet.asScala.toSet
  }.fold(e => 
    err(s"triplesPredicateObject: Error obtaining triples from predicate ${pred}: ${e.getLocalizedMessage()}"), 
    ok(_))

  def triplesObject(pred: JenaRDFNode, model: JenaModel): IO[Set[Statement]] =
  Try {
    model.listStatements(null, null, pred).toSet.asScala.toSet
  }.fold(e => 
      err(s"triplesObject: Error obtaining triples with object ${pred}: ${e.getLocalizedMessage()}"), 
      ok(_)
  )

  def triplesPredicateObject(property: Property,
                             obj: JenaRDFNode,
                             model: JenaModel
                            ): IO[Set[Statement]] = Try {
    model.listStatements(null, property, obj).toSet.asScala.toSet
  }.fold(
    e => err(s"triplesPredicateObject: Error obtaining triples with predicate: ${property} and object ${obj}: ${e.getLocalizedMessage()}"), 
    ok(_))

  // TODO: Return Either[String,Path]
  def path2JenaPath(path: SHACLPath, model: JenaModel, base: Option[IRI]): IO[Path] = {
    path match {
      case PredicatePath(iri) => for {
        prop <- rdfNode2Property(iri, model,base)
      } yield new P_Link(prop.asNode)
      
      case InversePath(path) => for {
        jenaPath <- path2JenaPath(path, model,base)
      } yield new P_Inverse(jenaPath)
      
      case SequencePath(paths) => {
        def seq(p1: Path, p2: Path): Path = new P_Seq(p1, p2)
        paths.toList.map(path2JenaPath(_, model,base)).sequence.map(_.reduce(seq))
      }
      case AlternativePath(paths) => {
        def alt(p1: Path, p2: Path): Path = new P_Alt(p1, p2)
        paths.toList.map(path2JenaPath(_, model,base)).sequence.map(_.reduce(alt))
      }
      case ZeroOrMorePath(path) => for {
        jenaPath <- path2JenaPath(path, model,base)
      } yield new P_ZeroOrMoreN(jenaPath)
      case ZeroOrOnePath(path) => for {
        jenaPath <- path2JenaPath(path, model,base)
      } yield new P_ZeroOrOne(jenaPath)
      case OneOrMorePath(path) => for {
        jenaPath <- path2JenaPath(path, model,base)
      } yield new P_OneOrMoreN(jenaPath)
    }
  }

  def wellTypedDatatype(node: RDFNode, expectedDatatype: IRI): IO[Boolean] = {
    node match {
      case l: es.weso.rdf.nodes.Literal => Try {
          val jenaLiteral = emptyModel.createTypedLiteral(l.getLexicalForm, l.dataType.str)
          jenaLiteral.getValue // if it is ill-typed it raises an exception
          jenaLiteral.getDatatypeURI
        }.fold(IO.raiseError(_), iri => ok(iri == expectedDatatype.str)) 
      case _ => ok(false)
    }
  }

  def resultSet2Map(rs: ResultSet): Either[String, List[Map[String,RDFNode]]] = {
    // var r: Either[String, Set[Map[String,RDFNode]]] = Right(Set())
    EitherUtils.sequence(rs.asScala.toList.map(querySolution2Map(_)))
  }

  def querySolution2Map(qs: QuerySolution): Either[String, Map[String,RDFNode]] = {
    var r: Either[String,Map[String, RDFNode]] = Right(Map())
    qs.varNames.forEachRemaining(x => for {
      node <- jenaNode2RDFNode(qs.get(x))
      } yield r = r.map(_.updated(x, node))
    )
    r
  }

  def shaclPath2JenaPath(p: SHACLPath): Path = {
    import org.apache.jena.sparql.path.PathFactory._
    p match {
      case PredicatePath(iri) => pathLink(iri2Property(iri).asNode)
      case InversePath(p) => pathInverse(shaclPath2JenaPath(p))
      case SequencePath(ps) => ps.map(shaclPath2JenaPath(_)).reduce(pathSeq(_,_))
      case AlternativePath(ps) => ps.map(shaclPath2JenaPath(_)).reduce(pathAlt(_,_))
      case ZeroOrMorePath(p) => pathZeroOrMore1(shaclPath2JenaPath(p))
      case ZeroOrOnePath(p) => pathZeroOrOne(shaclPath2JenaPath(p))
      case OneOrMorePath(p) => pathOneOrMore1(shaclPath2JenaPath(p))
    }
  }

}
