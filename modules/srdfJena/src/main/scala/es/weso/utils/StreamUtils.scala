package es.weso.utils

import cats.effect._
import fs2.Stream

object StreamUtils {

  def fromIOLs[A](ioLs: IO[List[A]]): Stream[IO,A] =  for {
      ls <- Stream.eval(ioLs)
      r <- Stream.emits(ls)
  } yield r 

}