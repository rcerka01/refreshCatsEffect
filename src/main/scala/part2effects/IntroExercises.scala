package part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.annotation.tailrec

// IO.defer(...)  is same as IO.delay(...).flatten (!!!)

object IntroExercises extends App {

  // 1
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    for {
      _ <- ioa
      b <- iob
    } yield b
  def sequenceTakeLastV2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // and then operator
  def sequenceTakeLastV3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob //call by name (just different method)

  // 2
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for {
      _ <- iob
      a <- ioa
    } yield a
  def sequenceTakeFirstV2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // 3
  def forever[A](ioa: IO[A]): IO[A] = // flat maps are just data structures. Sort of lazy
    ioa.flatMap( _ => forever(ioa))
  def foreverV2[A](ioa: IO[A]): IO[A] =
    ioa >> foreverV2(ioa) // lazy evaluation, always recommended
  def foreverV3[A](ioa: IO[A]): IO[A] =
    ioa *> foreverV3(ioa) // this will evaluate even before call. Stack overflow
  def foreverV4[A](ioa: IO[A]): IO[A] =
    ioa.foreverM // tail recursive

  // 4
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa map (_ => value)
  def convertV2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa as value

  def asUnit[A](ioa: IO[A]): IO[Unit] =
    for {
      _ <- ioa
    } yield ()
  def asUnitV2[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  def sumIO(n: Int): IO[Int] = {
    @tailrec
    def aux(cur: Int, acc: Int): Int =
      if (cur <= 0) acc
      else aux(cur - 1, acc + cur)
    IO.delay(aux(n, 0))
  }

  def fibonacci(n: Int): IO[BigInt] = {
    @tailrec
    def aux(cur: Int, prev: BigInt, acc: BigInt): BigInt =
      if (cur == n) acc
      else aux( cur + 1, acc, prev + acc )
    IO.delay(aux(1, 0, 1))
  }

  // TEST
  //forever(IO(println("forever"))).unsafeRunSync()
  println(fibonacci(100).unsafeRunSync())



}
