package part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOparalel extends IOApp.Simple {

  // SEQUENTIAL
  val oneIO: IO[String] = IO(s"[${Thread.currentThread().getName}] one")
  val twoIO: IO[String] = IO(s"[${Thread.currentThread().getName}] two")

  val composed: IO[String] =
    for {
      one <- oneIO
      two <- twoIO
    } yield s"$one and $two"


  // SEQUENTIAL WITH MAPn

  import utils._ // debugger from utils
  import cats.syntax.apply._ // for mapN

  val figurant1: IO[Int] = IO(42)
  val figurant2: IO[String] = IO("Scala")

  val composed2: IO[String] = (figurant1.debug, figurant2.debug).mapN((n, s) => s"this is now $n and $s")


  // PARALLEL

  val parFigurant1: IO.Par[Int] = Parallel[IO].parallel(figurant1.debug)
  val parFigurant2: IO.Par[String] = Parallel[IO].parallel(figurant2.debug)

  import cats.effect.implicits._
  val composedParallel: IO.Par[String] = (parFigurant1, parFigurant2).mapN((n, s) => s"this is now $n and $s")
  // then par output has to be turned back sequential
  val seqFromPar: IO[String] = Parallel[IO].sequential(composedParallel) // and it is now showing to come from different Threads

  // SHORTER PARALLEL

  import cats.syntax.parallel._
  val shorterParallel: IO[String] = (figurant1.debug, figurant2.debug).parMapN((n, s) => s"this is now $n and $s") // thats it

  // ERROR HANDLING

  val aFailureFirst: IO[String]  = IO.raiseError(new RuntimeException("First failure"))
  val aFailureSecond: IO[String] = IO.raiseError(new RuntimeException("Second failure"))

  val parWithFailure: IO[String] = (figurant1.debug, aFailureSecond.debug).parMapN((n, s) => s"this is now $n and $s")

  // first to fail will give a resulting failure
  val parWithTwoFailures: IO[String] = (IO(Thread.sleep(1000)) >> aFailureFirst.debug, aFailureSecond.debug).parMapN((n, s) => s"this is now $n and $s")


  override def run: IO[Unit] = {
    parWithTwoFailures.debug.void
  }
}
