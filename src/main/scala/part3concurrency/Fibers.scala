package part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome}

object Fibers extends IOApp.Simple {

  // SEQUENTIAL
  val figurant1: IO[Int] = IO.pure(12)
  val figurant2: IO[String] = IO.pure("Scala")

  import utils._ // for debug

  def seqThreadIOs() =
    for {
      _ <- figurant1.debug
      _ <- figurant2.debug
    } yield ()

  // FIBER
  def createFiber: Fiber[IO, Throwable, String] = ??? // that is its signature, but almost impossible to create it like that :)

  // not started, but allocated into effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = figurant1.debug.start

  def withFiberThreadIOs() =
    for {
      _ <- aFiber
      _ <- figurant2.debug
    } yield ()
//    [io-compute-3] 12
//    [io-compute-1] Scala
//    [io-compute-1] ()

  // JOINING FIBER
  // means wait for fiber to finish
  def withFiberJoinThreadIOs(): IO[Outcome[IO, Throwable, Int]] =
    for {
      fib    <- figurant1.debug.start // move to other thread
      result <- fib.join
    } yield (result)
//    [io-compute-4] 12
//    [io-compute-3] Succeeded(IO(12))

  // Outcome can be:
  // - Success
  // - Failure
  // - Cancelled

  // example to match outcome:
  val outcome: IO[Outcome[IO, Throwable, Int]] = withFiberJoinThreadIOs()
  outcome.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled()  => IO(0)
  }

  // example to throw error (Errored)
  def throwException() = {
    for {
      fib <- IO.raiseError[Int](new RuntimeException("Exception on")).start
      result <- fib.join
    } yield result
  }
  // [io-compute-3] Errored(java.lang.RuntimeException: Exception on)

  // example to cancel
  import scala.concurrent.duration._
  def cancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug // TO.sleep - spec function
    val taskWithCancelationHandler = task.onCancel( IO("Hallo from cancelation handler").debug.void )
    for {
      fib <- taskWithCancelationHandler.start
      _   <- IO.sleep(500.millisecond) >> IO("cancelling").debug
      _   <- fib.cancel
      result <- fib.join
    } yield result
  }

  // EXERCISES
  // 1.
  def processResultsFromFiber[A](io: IO[A]): IO[A] =
    (for {
      fib    <- io.start
      result <- fib.join
    } yield result) flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
    }

  // 2.
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    for {
      a <- processResultsFromFiber(ioa)
      b <- processResultsFromFiber(iob)
    } yield (a, b)

  // 3.
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    ( for {
      fib <- io.start
        - <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result ) flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
    }






  override def run: IO[Unit] =
    cancel().debug.void
}
