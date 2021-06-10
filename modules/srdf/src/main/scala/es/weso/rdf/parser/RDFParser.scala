package es.weso.rdf.parser

import es.weso.rdf.nodes._
import scala.util._
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.triples.RDFTriple
import cats.data._
import cats.implicits._
import cats.effect._
import fs2.Stream

private[weso] case class Config(node: RDFNode,rdf: RDFReader) 

/**
 * Obtains data from an RDFReader
 *
 * <p>
 * The approach is similar to parser combinators but instead of sequence of characters,
 * we have RDF graphs available through an RDFReader
 *
 */
trait RDFParser {

  /**
   * An RDFParser of values of type `a` takes a pointed node `RDFNode`
   * and an `RDFReader` and tries to obtain a value of type `a`
   */
  type RDFParser[A] = EitherT[R, Err, A] 
  type Err = Throwable

  type R[A] = ReaderT[IO,Config,A]


  def lift[A](r: R[A]): RDFParser[A] = EitherT.liftF(r)

  def liftIO[A](io: IO[A]): RDFParser[A] = {
    lift(ReaderT.liftF(io))
  }

  def getRDF: RDFParser[RDFReader] = {
    lift(ReaderT.ask[IO,Config].map(_.rdf))
  }

  def info(msg: String): RDFParser[Unit] = {
    lift(ReaderT.liftF(IO(println(msg))))
  }


  def getNode: RDFParser[RDFNode] = {
    lift(ReaderT.ask[IO,Config].map(_.node))
  }

  def fromEither[A](e: Either[Err,A]): RDFParser[A] = 
    EitherT.fromEither[R](e)

  def fromEitherT[A](e: EitherT[IO,Err,A]): RDFParser[A] = for {
    either <- liftIO(e.value)
    v <- either.fold(err => parseException(err), v => ok(v))
  } yield v

  def withNode[A](n: RDFNode, parser: RDFParser[A]): RDFParser[A] = {
    def localEnv(cfg: Config): Config = cfg.copy(node = n)
    val f: R[Either[Err,A]] = ReaderT.local(localEnv)(parser.value)
    EitherT(f)
  }

  def withRdf[A](rdf: RDFReader, parser: RDFParser[A]): RDFParser[A] = {
    val localEnv: Config => Config = _.copy(rdf = rdf)
    val f: R[Either[Err,A]] = ReaderT.local(localEnv)(parser.value)
    EitherT(f)
  }

  /**
   * An RDF parser that parses a value of type `a` if possible
   *
   */
  def optional[A](parser: RDFParser[A]): RDFParser[Option[A]] = {
    lift(parser.toOption.value)
  }


/*  implicit val applicativeRDFParser = new Applicative[RDFParser] {
    def pure[A](x: A) = (n, rdf) => Right(x)

    def ap[A, B](ff: RDFParser[A => B])(fa: RDFParser[A]): RDFParser[B] = (n, f) => {
      fa(n, f) match {
        case Right(a) => ff(n, f) match {
          case Right(f) => Right(f(a))
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
    }
  } */

  /*
   * returns an RDFParser which returns the IRI associated with a predicate
   * @param p predicate
   */
  def iriFromPredicate(p: IRI): RDFParser[IRI] =  
    for {
      node <- objectFromPredicate(p)
      iri <- node match {
        case i: IRI => parseOk(i)
        case _ => parseFail("Value of predicate " + p + " must be a IRI but it is: " + node)
      }
    } yield iri


  /**
   * Returns the String associated with a predicate `p`
   * @param p predicate
   * @return An RDFParser that returns the String associated with that predicate
   *
   */
  def stringFromPredicate(p: IRI): RDFParser[String] = 
    for {
      obj <- objectFromPredicate(p)
      str <- obj match {
        case StringLiteral(str) => parseOk(str)
        case _ => parseFail("Value of predicate " + p + " must be a string literal but it is: " + obj)
      }
    } yield str

  /**
    * Returns the Decimal literal associated with a predicate `p`
    * @param p predicate
    * @return An RDFParser that returns the decimal literal associated with that predicate
    *
    */
  def decimalLiteralFromPredicate(p: IRI): RDFParser[DecimalLiteral] = 
    for {
      obj <- objectFromPredicate(p)
      node <- obj match {
        case d: DecimalLiteral => parseOk(d)
        case _ => parseFail("Value of predicate " + p + " must be a decimal literal but it is: " + obj)
      }
    } yield node


  /**
   *
   */
  def stringFromPredicateOptional(p: IRI): RDFParser[Option[String]] =
    optional(stringFromPredicate(p))

  def objectFromPredicateOptional(p: IRI): RDFParser[Option[RDFNode]] =
    optional(objectFromPredicate(p))

  def decimalLiteralFromPredicateOptional(p: IRI): RDFParser[Option[DecimalLiteral]] =
    optional(decimalLiteralFromPredicate(p))

  /**
   * Returns a parser that obtains the type associated with the current node
   * <p>
   * Fails if there are more than one type associated
   */
  def rdfType: RDFParser[RDFNode] = 
    for {
      t <- objectFromPredicate(`rdf:type`)
    } yield t

  /**
   * Returns a parser that obtains the set of types associated
   * with the current node
   */
  def rdfTypes: RDFParser[Set[RDFNode]] =
    objectsFromPredicate(`rdf:type`)

  def io2r[A](x: IO[A]): R[A] = ReaderT.liftF[IO,Config,A](x)

  def fromIO[A](x: IO[A]): RDFParser[A] = EitherT.liftF(io2r(x))

  def stream2list[A](st: Stream[IO,A]): IO[Vector[A]] = st.compile.toVector
  // def ioVector[A](vs: IO[Vector[A]]): 
  
  def fromRDFStream[A](r: Stream[IO,A]): RDFParser[Vector[A]] = {
    fromIO(r.compile.toVector)
  }

  /**
   * RDFParser that retrieves the object associated with current node for a given predicate
   * <p>
   * Fails if there are more than one object
   *
   * @param p predicate
   */
  def objectFromPredicate(p: IRI): RDFParser[RDFNode] = 
   for {
    rdf <- getRDF
    n <- getNode 
    ts <- fromRDFStream(rdf.triplesWithSubjectPredicate(n,p))
    r <- ts.size match {
      case 0 => parseFail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => parseOk(ts.head.obj)
      case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
   } yield r

  /**
   * RDFParser that retrieves the set of iriObjects associated with the current node for a given predicate
   * <p>
   *
   * @param p predicate
   */
  def objectsFromPredicate(p: IRI): RDFParser[Set[RDFNode]] = 
   for {
    rdf <- getRDF
    n <- getNode
    triples <- fromRDFStream(rdf.triplesWithSubjectPredicate(n, p))
    r <- parseOk(objectsFromTriples(triples.toSet))
   } yield r
  

  /**
   * A parser of the RDF List associated with the current node
   * <p>
   * Fails if there are more than one iriObjects associated with `rdf_first` or `rdf_rest`
   */
  def rdfList: RDFParser[List[RDFNode]] = 
    rdfListAux(Vector())

  def rdfListAux(visited: Vector[RDFNode]): RDFParser[List[RDFNode]] = for {
    n <- getNode
    ls <- n match {
      case `rdf:nil` => parseOk(List())
      case _ => for {
          elem <- objectFromPredicate(`rdf:first`)
          next <- objectFromPredicate(`rdf:rest`)
          ls <- if (visited contains next)
                  parseFail(s"Parsing RDF list with circular structure on node $next")   
                else 
                  withNode(next, rdfListAux(visited :+ next))
        } yield (elem :: ls)
      }
  } yield ls

  /**
   * Obtains the RDF list associated with a predicate for the current node
   *
   * @param p predicate
   */
  def rdfListForPredicate(p: IRI): RDFParser[List[RDFNode]] = 
    for {
      value <- objectFromPredicate(p)
      ls <- withNode(value, rdfList)
    } yield ls

  /**
    * Obtains the RDF list associated with a predicate for the current node
    * If there is no value, returns the empty list
    *
    * @param p predicate
    */
  def rdfListForPredicateAllowingNone(p: IRI): RDFParser[List[RDFNode]] = 
    for {
      maybeValue <- objectFromPredicateOptional(p)
      ls <- maybeValue match {
        case None => parseOk(List())
        case Some(value) => withNode(value, rdfList)
      }
    } yield ls

  /**
   * Obtains an integer literal associated with a predicate in the current node
   *
   * @param p predicate
   */
  def integerLiteralForPredicate(p: IRI): RDFParser[Int] = 
    for {
      n <- getNode
      rdf <- getRDF
      ts <- fromRDFStream(rdf.triplesWithSubjectPredicate(n, p))
      r <- ts.size match {
        case 0 => parseFail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
        case 1 => getIntegerLiteral(ts.head)
        case _ => parseFail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
      }
    } yield r

  def integerLiteralsForPredicate(p: IRI): RDFParser[List[Int]] = 
   for {
    rdf <- getRDF
    n <- getNode
    ts <- fromRDFStream(rdf.triplesWithSubjectPredicate(n, p))
    // val zero : Either[String,List[Int]] = Right(List())
    r <- fromEither {
      def cmb(ls: Either[Throwable, List[Int]], node: RDFNode): Either[Throwable, List[Int]] =
      ls.fold(e => Left(e),
                vs =>
                  node match {
                    case i: IntegerLiteral => Right(i.int :: vs)
                    case _                 => Left(RDFException.fromString(s"node $node must be an integer literal"))
                })
      ts.map(_.obj).foldLeft(List[Int]().asRight[Throwable])(cmb)
    }
   } yield r

  /**
   * Returns `true` if the current node does not have a given type
   *
   * @param t type to be checked
   */
  def hasNoRDFType(t: IRI): RDFParser[Boolean] = 
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)
    } yield !declaredTypes.contains(t)

  /**
   * Returns `true` if the current node has a given type
   *
   * @param t type to be checked
   */
  def hasRDFType(t: IRI): RDFParser[Boolean] = 
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)
    } yield declaredTypes.contains(t)

  /**
   * Returns `true` if the current node has a type which belong to a given set of types
   *
   * @param ts set of types to be checked
   */
  def hasSomeRDFType(ts: Set[IRI]): RDFParser[Boolean] = 
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)
    } yield {
      val iriTypes = declaredTypes.collect { case i: IRI => i}
      iriTypes.diff(ts).size > 0
    }

  
  /**
   * Checks if some of the parsers pass and returns the corresponding value
   *
   * @param ps sequence of parsers
   */
  def someOf[A](ps: RDFParser[A]*): RDFParser[A] = 
    {
      val zero: RDFParser[A] = fromEither(RDFException.fromString("someOf: none of the RDFParsers passed").asLeft[A])
      def cmb(c: RDFParser[A], parser: RDFParser[A]): RDFParser[A] = 
        c orElse parser
      
      ps.foldLeft(zero)(cmb)
    }

  /**
   * Applies a parser over a sequence of nodes
   *
   * @param parser parser
   * @param nodes sequence of nodes
   */
  def group[A](
    parser: RDFParser[A],
    nodes: Seq[RDFNode]): RDFParser[Seq[A]] = {
      val empty: RDFParser[Seq[A]] = ok(Seq())
      def cmb(node: RDFNode, rest: RDFParser[Seq[A]]): RDFParser[Seq[A]] = for {
        rs <- rest
        v <- withNode(node,parser)
      } yield v +: rs
      nodes.foldRight(empty)(cmb)
    }

  /**
   * Applies a list of parsers
   * If a parser fails, it continues with the rest of the list
   * @return the list of successful values that can be parsed
   */
  def anyOf[A](ps: RDFParser[A]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[A]): RDFParser[Seq[A]] = for {
      maybe <- p.map(_.some) orElse ok(none[A])
      rs <- rest 
    } yield maybe match {
      case None => rs
      case Some(x) => x +: rs
    }      
    val zero: RDFParser[Seq[A]] = ok(Seq())
    ps.foldLeft(zero)(comb)
  }
  /**
  * Applies a list of parsers
    * @param ps: List of parsers. Each parser returns a list of values
    * @tparam A
    */
  def anyOfLs[A](ps: RDFParser[List[A]]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[List[A]]): RDFParser[Seq[A]] = for {
      maybe <- p.map(_.some) orElse ok(none[List[A]])
      rs <- rest 
    } yield maybe match {
      case None => rs
      case Some(xs) => xs ++ rs
    } 
    val zero: RDFParser[Seq[A]] = ok(Seq())
    ps.foldLeft(zero)(comb)
  }

  /**
   * If a parser fails, it continues with the rest of the list
   * @return the result of the first parser that succeeds of failure
   *
   */
  def firstOf[A](ps: RDFParser[A]*): RDFParser[A] = {
    def comb(rest: RDFParser[A], p: RDFParser[A]): RDFParser[A] = 
      p orElse rest
/*      p(n, rdf) match {
        case Left(_) => rest(n, rdf)
        case Right(x) => Right(x)
      }
    } */
    val zero: RDFParser[A] = parseFail("firstOf: none of the parsers succeeded")
    ps.foldLeft(zero)(comb)
  }

  /**
   * Checks that exactly one of the parsers succeeds on the current node
   *
   * @param parsers sequence of parsers
   */
  def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { 
    {
      val zero: RDFParser[A] = parseFail("oneOf: none of the RDFParsers passed")
      def cmb(c: RDFParser[A], parser: RDFParser[A]): RDFParser[A] = 
         c.biflatMap(_ => parser, 
            value => parser.biflatMap(_ => ok(value), 
            value2 => parseFail(s"oneOf: More than one parser passes. Two vaules that pass: $value and $value2")))
      /*{
        c.fold(
          _ => parser(n,rdf),
          _ => parser(n,rdf).
            fold(
              _ => c,
              _ => "oneOf: More than one parser passes".asLeft[A])
        )
      } */
      parsers.foldLeft(zero)(cmb)
    }
  }


  def subjectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map(_.subj) // { case t: RDFTriple => t.subj }
  }

  def objectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map(_.obj) // { case t: RDFTriple => t.obj }
  }

  def getIntegerLiteral(t: RDFTriple): RDFParser[Int] = {
    t.obj match {
      case l: IntegerLiteral => parseOk(l.int)
      // TODO: case l: DatatypeLiteral(lexicalForm,datatype) => ...
      case _ => parseFail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }

  /**
   * Combine a sequence of RDFParsers
   *
   */
  def combineAll[A](ps: RDFParser[Seq[A]]*): RDFParser[Seq[A]] = {
    val zero: RDFParser[Seq[A]] = ok(Seq())
    ps.foldLeft(zero)(combine)
  }

  def combine[A](p1: RDFParser[Seq[A]], p2: RDFParser[Seq[A]]): RDFParser[Seq[A]] = 
    for {
      vs1 <- p1
      vs2 <- p2
    } yield {
      vs1 ++ vs2
    }

  def mapRDFParser[A, B](ls: List[A], p: A => RDFParser[B]): RDFParser[List[B]] = {
    ls.map(v => p(v)).sequence[RDFParser,B]
  }

  def rdfListForPredicateOptional(p: IRI): RDFParser[List[RDFNode]] = for {
    maybeLs <- optional(rdfListForPredicate(p))
  } yield maybeLs.fold(List[RDFNode]())(ls => ls)

  def literalFromPredicate(p: IRI): RDFParser[Literal] = for {
    o <- objectFromPredicate(p)
    r <- withNode(o,asLiteral)
  } yield r

  def asLiteral: RDFParser[Literal] = for { 
    n <- getNode 
    v <- n match {
      case l: Literal => parseOk(l)
      case _ => parseFail(s"Expected node $n to be a literal")
     } 
    } yield v

  def asLiterals(ls: List[RDFNode]): RDFParser[List[Literal]] = 
    ls.map(withNode(_, asLiteral)).sequence

  def literalsFromPredicate(p: IRI): RDFParser[List[Literal]] = for {
    os <- objectsFromPredicate(p)
    r <- asLiterals(os.toList)
  } yield r

  def boolean: RDFParser[Boolean] = for {
    n <- getNode
    v <- n match {
     case BooleanLiteral.trueLiteral => parseOk(true)
     case BooleanLiteral.falseLiteral => parseOk(false)
     case DatatypeLiteral("true", `xsd:boolean`) => parseOk(true)
     case DatatypeLiteral("false", `xsd:boolean`) => parseOk(false)
     case _ => parseFail(s"Expected boolean literal. Found $n")
   }
  } yield v

  def iri: RDFParser[IRI] = for {
    n <- getNode
    v <- n match {
    case i: IRI => parseOk(i)
    case _ => parseFail(s"Expected IRI, found $n")
   }
  } yield v

  def integer: RDFParser[Int] = for {
    n <- getNode
    v <-  n match {
      case l: IntegerLiteral => parseOk(l.int)
      case _ => parseFail(s"Expected integer literal for node $n")
    }
  } yield v

  def string: RDFParser[String] = for { 
    n <- getNode
    v <- n match {
    case s: StringLiteral => parseOk(s.getLexicalForm)
    case _ => parseFail(s"Expected string literal for node $n")
   }
  } yield v

  def booleanFromPredicate(p: IRI): RDFParser[Boolean] = arc(p, boolean)

  def booleanFromPredicateOptional(p: IRI): RDFParser[Option[Boolean]] = for {
    maybeObj <- objectFromPredicateOptional(p)
    v <- maybeObj match {
      case None => ok(None)
      case Some(BooleanLiteral(b)) => ok(Some(b))
      case Some(o) => parseFail(s"value of $p must be a boolean literal. Obtained $o")
    }
  } yield v

  def irisFromPredicate(p: IRI): RDFParser[List[IRI]] = for {
    os <- objectsFromPredicate(p)
    is <- nodes2iris(os.toList) match {
      case Right(iris) => ok(iris)
      case Left(msg) => parseFail(msg)
    }
  } yield is

  def iriFromPredicateOptional(p: IRI): RDFParser[Option[IRI]] =  
    optional(iriFromPredicate(p))


  def opt[A](pred: IRI, parser: RDFParser[A]): RDFParser[Option[A]] = for {
    n <- getNode
    os <- objectsFromPredicate(pred)
    v <- os.size match {
        case 0 => ok(None)
        case 1 => withNode(os.head, parser).map(Some(_))
        case _ => parseFail(s"opt fails because $n has more than one value for pred $pred. Values: $os")
      }
    } yield v orElse None

  def starWithNodes[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[(RDFNode, A)]] = 
    for {
      os <- objectsFromPredicate(pred).map(_.toList)
      vs <- parseNodes(os, parser)
    } yield os zip vs

  def star[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] = 
    for {
      os <- objectsFromPredicate(pred).map(_.toList)
      vs <- parseNodes(os, parser)
    } yield vs

  def collect[A](ps: List[RDFParser[A]]): RDFParser[List[A]] = {
    val zero: RDFParser[List[A]] = ok(List())
    def combine(xs: RDFParser[List[A]], parser: RDFParser[A]): RDFParser[List[A]] = {
      parser.biflatMap(_ => xs, x => for {
        v <- xs  
      } yield x :: v)
    }
    ps.foldLeft(zero)(combine)
  }

  def checkType(expected: RDFNode): RDFParser[Boolean] = for {
    n <- getNode
    obtained <- objectFromPredicate(`rdf:type`)
    v <- if (obtained == expected) ok(true)
    else
      parseFail(s"Type of node $n must be $expected but obtained $obtained")
  } yield v

  def condition(cond: RDFNode => Boolean, name: String): RDFParser[RDFNode] = for {
    n <- getNode
    b <- if (cond(n)) ok(n)
         else parseFail(s"Condition $name not satisfied on node $n")
  } yield b

  def failIf(cond: Boolean, msg: String): RDFParser[Unit] = for {
    n <- getNode
    b <- if (cond) parseFail(s"Condition failed: $msg. Current node: $n")
         else ok(())
  } yield b

  def arc[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = for {
    obj <- objectFromPredicate(pred)
    x <- withNode(obj,parser)
  } yield x

  /**
   * Parses a list of values. The list must contain at least two values
   */
  def list2Plus[A](p: RDFParser[A]): RDFParser[List[A]] = for {
    first <- arc(`rdf:first`, p)
    restNode <- objectFromPredicate(`rdf:rest`)
    rest <- withNode(restNode, list1Plus(p))
  } yield first :: rest

  /**
   * Parses a list of values. The list must contain at least one value
   */
  def list1Plus[A](p: RDFParser[A]): RDFParser[List[A]] = 
    list1PlusAux(p, List())

  def list1PlusAux[A](p: RDFParser[A], visited: List[RDFNode]): RDFParser[List[A]] = 
   for {
    first <- arc(`rdf:first`, p)
    restNode <- objectFromPredicate(`rdf:rest`)
    rest <- parseRest(visited, restNode, p)
  } yield first :: rest

  def parseRest[A](
    visited: List[RDFNode],
    restNode: RDFNode,
    parser: RDFParser[A]): RDFParser[List[A]] = if (restNode == `rdf:nil`) parseOk(List[A]())
    else if (visited contains restNode)
      parseFail(s"Parsing list with recursive nodes. visitedNodes: $visited, node: $restNode")
    else
      withNode(restNode, list1PlusAux(parser, restNode :: visited))

  def rdfNil[A]: RDFParser[List[A]] = for {
    n <- getNode
    v <- if (n == `rdf:nil`) parseOk(List())
         else parseFail(s"Expected rdf_nil but got $n")
  } yield v

  def nodes2iris(ns: List[RDFNode]): Either[String, List[IRI]] = {
    sequenceEither(ns.map(_.toIRI))
  }

  // Todo: Use "sequence" when I find why it gives a type error...
  def sequenceEither[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = {
    val zero: Either[E, List[A]] = Either.right(List())
    def next(r: Either[E, List[A]], x: Either[E, A]): Either[E, List[A]] =
      x match {
        case Left(e) => Left(e)
        case Right(v) => r match {
          case Left(e) => Left(e)
          case Right(vs) => Right(v :: vs)
        }
      }
    xs.foldLeft(zero)(next)
  }

  def ok[A](x: A): RDFParser[A] = 
    parseOk(x)

  def parseFail[A](str: String): RDFParser[A] = 
    EitherT[R,Err,A](ReaderT.pure(Left(RDFException.fromString(str))))

  def parseException[A](e: Throwable): RDFParser[A] =
    EitherT[R,Err,A](ReaderT.pure(Left(RDFException.fromException(e))))


  def parseOk[A](x: A): RDFParser[A] =
    EitherT.pure(x)

  def parseNodes[A](nodes: List[RDFNode], parser: RDFParser[A]
  ): RDFParser[List[A]] = 
    nodes.map(withNode(_, parser)).sequence

  def parsePredicateLiteralList[A](p: IRI, maker: Literal => A): RDFParser[List[A]] = 
   for {
    vs <- literalsFromPredicate(p)
   } yield vs.map(maker(_))

  def parsePredicateLiteral[A](p: IRI, maker: Literal => A): RDFParser[A] = for {
    v <- literalFromPredicate(p)
  } yield maker(v)

  def parsePredicateInt[A](p: IRI, maker: Int => A): RDFParser[A] = for {
    v <- integerLiteralForPredicate(p)
  } yield maker(v.intValue())

  def parsePredicateIntList[A](p: IRI, maker: Int => A): RDFParser[List[A]] = for {
    vs <- integerLiteralsForPredicate(p)
  } yield vs.map(maker(_))

  def parsePredicateString[A](p: IRI, maker: String => A): RDFParser[A] = for {
    v <- stringFromPredicate(p)
  } yield maker(v)

  def parsePredicateList[A](p: IRI, maker: RDFNode => A): RDFParser[List[A]] = 
    for {
      os <- objectsFromPredicate(p)
    } yield os.toList.map(o => maker(o))

  def parsePredicate[A](p: IRI, maker: RDFNode => A): RDFParser[A] = 
    for {
      o <- objectFromPredicate(p)
    } yield maker(o)

  def parsePredicateIRI[A](p: IRI, maker: IRI => A): RDFParser[A] = for {
    iri <- iriFromPredicate(p)
  } yield maker(iri)

  def parsePredicateIRIList[A](p: IRI, maker: IRI => A): RDFParser[List[A]] = for {
    iris <- irisFromPredicate(p)
  } yield iris.map(maker(_))


  def parse[A](parser: RDFParser[A], node: RDFNode, rdf: RDFReader): IO[Either[Err,A]] = {
    parser.value.run(Config(node,rdf))
  }

}
