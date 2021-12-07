package part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling extends App {

  val aFailed1: IO[Int] = IO.delay(throw new RuntimeException("Failed"))
  val aFailed2: IO[Int] = IO.raiseError(new RuntimeException("Failed")) // both are equal, but this one is nicer

  // handle
  aFailed2.handleErrorWith {
    case _: RuntimeException => IO.delay(print("That and that happen"))
  }

  // transform
  val toEither: IO[Either[Throwable, Int]] = aFailed2.attempt

  // redeem
  val redeem: IO[String] = aFailed2.redeem( e => s"FIL: $e", value => s"SUCCESS: $value")
  val redeemWith: IO[Unit] = aFailed2.redeemWith( e => IO(println(s"FIL: $e")), value => IO(println(s"SUCCESS: $value")))


  // EXERCISES
  // option
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case None => IO.raiseError(ifEmpty)
    case Some(v) => IO(v)
  }

  // Try
  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Failure(e) => IO.raiseError(e)
    case Success(v) => IO(v)
  }

  // Either
  def either2IO[A](either: Either[Throwable, A]): IO[A] = either match {
    case Left(e) => IO.raiseError(e)
    case Right(v) => IO(v)
  }

  // handle error
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)


}
