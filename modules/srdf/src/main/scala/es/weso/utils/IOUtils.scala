package es.weso.utils
import cats._
import cats.effect._
import cats.implicits._
import fs2.Stream
import es.weso.utils.internal.CollectionCompat._

case class IOException(msg: String, exc: Option[Throwable]) extends Exception

object IOException {
    def fromString(msg: String): IOException = IOException(msg, None)
}


object IOUtils {

 def fromES[A](e: Either[String,A]): IO[A] = 
    IO.fromEither(e.leftMap(IOException.fromString(_)))

 def err[A](msg: String): IO[A] = 
     IO.raiseError(IOException.fromString(msg))

 def ok[A](x: A): IO[A] = IO(x)

 def errStream[A](msg: String): Stream[IO,A] =
     Stream.raiseError[IO](IOException.fromString(msg))

 def streamFromLazyList[A](ls: LazyList[A]): Stream[IO,A] = Stream.emits(ls)

 def fromIO[A](x: IO[A]): Stream[IO,A] = Stream.eval(x)

 def sequence[A](vs: List[IO[A]]): IO[List[A]] = vs.sequence

 def io2ES[A](v: IO[A]):Either[String,A] = 
   MonadError[IO,Throwable].attempt(v).unsafeRunSync().leftMap(_.getMessage())
  
 // The following code is inspired by: 
 // https://stackoverflow.com/questions/49751533/how-to-convert-a-streamio-lista-to-streamio-a
 def streamFromIOs[A](vs: IO[List[A]]): Stream[IO,A] = { 
     Stream.eval(vs).flatMap(x => Stream(x: _*))
 } 

}