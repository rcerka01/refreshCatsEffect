package part2effects

import scala.io.StdIn

object IOintroduction extends App {

  import cats.effect.IO

  // INTRO
  val pureVal: IO[Int] = IO.pure(42) // when known there is no side effect
  val notPure: IO[Int] = IO.delay { // when not sure (it will evaluate later)
    println("Sideefect")
    53
  }
  val notPure2: IO[Int] = IO { // apply, same as delay
    println("Sideefect 2")
    64
  }

  import cats.effect.unsafe.implicits.global // to run
  notPure2.unsafeRunAsync( println ) // to run

  // TRANSFORM
  pureVal map (_ * 2)
  pureVal flatMap( a => IO.delay(println(a)))

  def smallProgram(): IO[Unit] = {
    for {
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
          _ <- IO.delay(println(line1 + line2))
    } yield ()
  }

  // smallProgram().unsafeRunSync()

  // COMBINE
  import cats.syntax.apply._

  val combineStuff: IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())) mapN ( _ + _) map println
  combineStuff.unsafeRunSync()


}
