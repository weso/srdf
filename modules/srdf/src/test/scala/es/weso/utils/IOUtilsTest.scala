package es.weso.utils

import org.scalatest._
import matchers.should._
import funspec._
import es.weso.utils.IOUtils._
import cats.effect.IO
import fs2.Stream
import es.weso.utils.internal.CollectionCompat._

class IOUtilsTest extends AnyFunSpec with Matchers {

  describe("IOUtils") {

    describe("ESIO") {
       it(s"SHould be able to add two numbers") {
        val r: ESIO[Int] = for {
            v1 <- ok_es(3)
            v2 <- ok_es(5)
        } yield v1 + v2

        run_es(r).unsafeRunSync should be(Right(8))
      }

      it(s"SHould fail when one fails") {
        val r: ESIO[Int] = for {
            v1 <- ok_es(3)
            v2 <- fail_es[Int]("No number")
            v3 <- ok_es(4)
        } yield v1 + v2 + v3

        run_es(r).unsafeRunSync should be(Left("No number"))
      }

      it(s"SHould fail when one fails in IO") {
        val r: IO[Int] = for {
            v1 <- ok(3)
            v2 <- err[Int]("No number")
            v3 <- ok(4)
        } yield v1 + v2 + v3
        an [IOException] should be thrownBy (r.unsafeRunSync)
      }

      it(s"Should convert io IO(3) to Right(3)") {
          val r = IO(3)
          io2ES(r) should be(Right(3))
      }

      it(s"Should convert io IO(exc...) to Left(exc)") {
          val r = IO.raiseError(new RuntimeException("Exc"))
          io2ES(r) should be(Left("Exc"))
      }

    }

    describe(s"Stream IO") {
      it(s"SHould convert lazy list to stream") {
        val ls: LazyList[Int] = LazyList(1,2,3)
        val r = streamFromLazyList(ls)
        r.compile.toList.unsafeRunSync should contain theSameElementsAs(List(1,2,3))
      }
    }

    describe(s"EitherT[IO,String,A]") {

        it(s"Happy path adding numbers") {
         val r: ESIO[Int] = for {
                v1 <- ok_es(1)
                v2 <- ok_es(2)
                v3 <- ok_es(3)
         } yield v1 + v2 + v3
         run_es(r).unsafeRunSync should be(Right(6))
        }

        it(s"Happy path adding from either and from IO") {
         val r: ESIO[Int] = for {
                v1 <- ok_es(1)
                v2 <- either2es(Right(2))
                v3 <- io2es(IO(3))
         } yield v1 + v2 + v3
         run_es(r).unsafeRunSync should be(Right(6))
        }

        it(s"Happy path adding from either, from IO and fromStream") {
         val r: ESIO[Int] = for {
                v1 <- ok_es(1)
                v2 <- either2es(Right(2))
                v3 <- io2es(IO(3))
                v4 <- stream2es(Stream(1,2,3).covary[IO])
         } yield v1 + v2 + v3 + v4.fold(0)(_ + _)
         run_es(r).unsafeRunSync should be(Right(12))
        }

    }
  }

}