package es.weso.rdf.rdf4j

import es.weso.rdf.nodes._
import es.weso.rdf.triples._
import scala.jdk.CollectionConverters._
import org.eclipse.rdf4j.model.{BNode => BNode_RDF4j, IRI => IRI_RDF4j, Literal => Literal_RDF4j, _}
import org.eclipse.rdf4j.model.impl.{SimpleValueFactory, BooleanLiteral => BooleanLiteral_RDF4j, DecimalLiteral => DecimalLiteral_RDF4j, IntegerLiteral => IntegerLiteral_RDF4j}
import org.eclipse.rdf4j.model.util.ModelBuilder
import org.eclipse.rdf4j.model.vocabulary.XMLSchema
import cats.effect.IO
import cats.implicits._
import es.weso.utils.IOUtils._

import scala.util.{Failure, Success, Try}

object RDF4jMapper {

  lazy val valueFactory = SimpleValueFactory.getInstance;

  def literal2Literal(lit: Literal_RDF4j): Literal = {
    lit match {
      case bl: BooleanLiteral_RDF4j => BooleanLiteral(bl.booleanValue())
      case il: IntegerLiteral_RDF4j => IntegerLiteral(il.intValue())
      case dl: DecimalLiteral_RDF4j => DecimalLiteral(dl.decimalValue())
      case _ if (lit.getDatatype == XMLSchema.STRING) => StringLiteral(lit.stringValue())
      case _ if (lit.getDatatype == XMLSchema.BOOLEAN) => BooleanLiteral(lit.booleanValue())
      case _ if (lit.getDatatype == XMLSchema.INTEGER) => IntegerLiteral(lit.integerValue().intValue())
      case _ if (lit.getDatatype == XMLSchema.DECIMAL) => DecimalLiteral(lit.decimalValue())
      case _ if (lit.getDatatype == XMLSchema.DOUBLE) => DoubleLiteral(lit.doubleValue())
      case _ if (lit.getLanguage.isPresent) => LangLiteral(lit.stringValue, Lang(lit.getLanguage.get()))
      case _ => DatatypeLiteral(lit.stringValue(), iri2iri(lit.getDatatype))
    }
  }

  def iri2iri(iri: IRI_RDF4j): IRI = IRI(iri.toString)

  def bnode2Bnode(b: BNode_RDF4j): BNode = BNode(b.getID)

  def value2RDFNode(value: Value): RDFNode = {
    value match {
      case lit: Literal_RDF4j => literal2Literal(lit)
      case bnode: BNode_RDF4j => bnode2Bnode(bnode)
      case iri: IRI_RDF4j => iri2iri(iri)
    }
  }

  def statement2RDFTriple(s: Statement): IO[RDFTriple] = for {
    rdfNode <- resource2RDFNode(s.getSubject)
  } yield RDFTriple(rdfNode, iri2iri(s.getPredicate), value2RDFNode(s.getObject))

  def resource2RDFNode(r: Resource): IO[RDFNode] = IO {
    r match {
      case iri: IRI_RDF4j => iri2iri(iri)
      case bnode: BNode_RDF4j => bnode2Bnode(bnode)
      case lit: Literal_RDF4j => literal2Literal(lit)
    }
  }

  def iri2Property(iri: IRI): IRI_RDF4j = {
    valueFactory.createIRI(iri.str)
  }

  def rdfNode2Resource(r: RDFNode): IO[Resource] = {
    r match {
      case iri: IRI => ok(valueFactory.createIRI(iri.str))
      case bnode: BNode => ok(valueFactory.createBNode(bnode.id))
      case _ => err(s"Cannot convert rdfNode: $r to Resource")
    }
  }

 def rdfNode2Value(r: RDFNode): Value = r match {
   case iri: IRI => iri2Property(iri)
   case bnode: BNode => valueFactory.createBNode(bnode.id)
   case StringLiteral(str) => valueFactory.createLiteral(str)
   case BooleanLiteral(b) => valueFactory.createLiteral(b)
   case IntegerLiteral(_, repr) => valueFactory.createLiteral(repr)
   case DecimalLiteral(_, repr) => valueFactory.createLiteral(repr)
   case DoubleLiteral(_, repr) => valueFactory.createLiteral(repr)
   case DatatypeLiteral(l,d) => valueFactory.createLiteral(l,iri2Property((d)))
   case LangLiteral(l,Lang(lang)) => valueFactory.createLiteral(l,lang)
 }

 def newBNode(): BNode_RDF4j = valueFactory.createBNode()

 def statements2RDFTriples(statements: Set[Statement]): IO[List[RDFTriple]] = {
    statements.toList.map(statement2RDFTriple).sequence
  }

  private[rdf4j] def triplesSubject(resource: Resource, model: Model): IO[Set[Statement]] = IO {
    model.filter(resource, null, null).asScala.toSet
  }

  private[rdf4j] def triplesPredicate(iri: IRI_RDF4j, model: Model): IO[Set[Statement]] = IO {
    model.filter(null, iri, null).asScala.toSet
  }

  private[rdf4j] def triplesObject(value: Value, model: Model): IO[Set[Statement]] = IO {
    model.filter(null, null, value).asScala.toSet
  }

  private[rdf4j] def triplesPredicateObject(iri: IRI_RDF4j, obj: Value, model: Model): IO[Set[Statement]] = IO {
    model.filter(null, iri, obj).asScala.toSet
  }

  private[rdf4j] def rdfTriples2Model(triples: Set[RDFTriple]): IO[Model] = for {
    ss <- triples.map(rdfTriple2Statement).toList.sequence
  } yield {
    val builder: ModelBuilder = new ModelBuilder
    ss.foreach(s => builder.add(s.getSubject, s.getPredicate, s.getObject))
    builder.build
  }

  private[rdf4j] def rdfTriple2Statement(triple: RDFTriple): IO[Statement] = {
    val pred = iri2Property(triple.pred)
    val obj = rdfNode2Value(triple.obj)
    for {
      subj <- rdfNode2Resource(triple.subj)
    } yield valueFactory.createStatement(subj, pred, obj)
  }

  // TODO: Check rules of datatype
  private[rdf4j] def wellTypedDatatype(node: RDFNode, expectedDatatype: IRI): IO[Boolean] = node match {
    case l: Literal => Try {
      val datatypeIRI = valueFactory.createIRI(l.dataType.str)
      val rdf4jLiteral = valueFactory.createLiteral(l.getLexicalForm, datatypeIRI)
      // val x = rdf4jLiteral.getLabel
      rdf4jLiteral.getDatatype
    } match {
      case Success(iri) => ok(iri.stringValue == expectedDatatype.str)
      case Failure(e) => err(e.getMessage)
    }
    // case DatatypeLiteral(_,dt) => Right(dt == expectedDatatype)
    case _ => ok(false)
  }

}
