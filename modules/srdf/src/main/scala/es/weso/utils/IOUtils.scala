package es.weso.utils
import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import fs2.Stream
import es.weso.utils.internal.CollectionCompat._

case class IOException(
    msg: String, 
    exc: Option[Throwable]
) extends Exception(msg)

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

     type ESIO[A] = EitherT[IO,String,A]

    def either2io[A](e: Either[String,A]): IO[Either[String,A]] = 
      e.fold((msg: String) => IO(msg.asLeft[A]), (v: A) => IO(v.asRight[String]) ) 

    def ok_es[A](x:A): ESIO[A] = EitherT.pure(x)
    def fail_es[A](msg: String): ESIO[A] = EitherT.fromEither(msg.asLeft[A])
    def io2es[A](io:IO[A]): ESIO[A] = EitherT.liftF(io)
    def either2es[A](e:Either[String,A]): ESIO[A] = EitherT.fromEither(e)
    def stream2es[A](s: Stream[IO,A]): ESIO[LazyList[A]] = io2es(s.compile.to(LazyList))
    def print_es[A](msg:String): ESIO[Unit] = io2es(IO(println(msg)))
    def run_es[A](es: ESIO[A]): IO[Either[String,A]] = es.value

    type ESF[A,F[_]] = EitherT[F,String,A]

    def ok_esf[A, F[_]:Applicative](x:A): ESF[A,F] = EitherT.pure[F,String](x)
    def fail_ef[A, F[_]:Applicative](msg: String): ESF[A,F] = EitherT.fromEither[F](msg.asLeft[A])
    def f2es[A, F[_]:Applicative](fa: F[A]): ESF[A,F] = EitherT.liftF(fa)
    def io2esf[A,F[_]:Effect: LiftIO](io:IO[A]): ESF[A,F] = EitherT.liftF(LiftIO[F].liftIO(io))
    def either2ef[A,F[_]:Applicative](e:Either[String,A]): ESF[A,F] = EitherT.fromEither[F](e)
    def stream2ef[A,F[_]:Effect](s: Stream[F,A]): ESF[LazyList[A],F] = f2es(s.compile.to(LazyList))

    def run_esf[A,F[_]](es: ESF[A,F]): F[Either[String,A]] = es.value
    def run_esiof[A,F[_]:Effect](esio: ESIO[A]): F[Either[String,A]] = run_esf(esio2esf[A,F](esio))

    def esio2esf[A, F[_]: Effect](e: ESIO[A]): ESF[A,F] = {
      for {
        either <- io2esf[Either[String,A],F](e.value)
        r <- either2ef[A,F](either)  
      } yield r
    }

    def io2f[A,F[_]:LiftIO](io: IO[A]): F[A] = LiftIO[F].liftIO(io)

    def stream2io[A](s: Stream[IO,A]): IO[LazyList[A]] = s.compile.to(LazyList)

}