package part2effects

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object IOtraversal extends IOApp.Simple {

  // BASIC EXAMPLE WITH FUTURES
  def someHeavyComputation(s: String): Future[Int] =
    Future {
      Thread.sleep(500)
      s.split(" ").length
    }

  def someWorkload = List("bla bla bla", "bla bla", "bla bla bla bla bla" )

  // HARD TO PROCESS STUFF
  // List[Future[Int]]]
  val hard: List[Future[Int]] = someWorkload map someHeavyComputation
  // to process => hard foreach ( _.foreach( println ))


  // EASY TO PROCESS STUFF
  // Future[List[Int]]]
  // use traverse
  import cats.Traverse
  import cats.instances.list._
  val listTraverse: Traverse[List] = Traverse[List]
  val easy: Future[List[Int]] = listTraverse.traverse(someWorkload)(someHeavyComputation)
  // to process => easy foreach println


  // IO TRAVERSE EXAMPLE
  def someHeavyComputationIO(s: String): IO[Int] =
    IO {
      Thread.sleep(500)
      s.split(" ").length
    }.debug
  def someWorkloadIO: List[IO[Int]] = someWorkload map someHeavyComputationIO
  val traversed: IO[List[Int]] = listTraverse.traverse(someWorkload)(someHeavyComputationIO)

  // IO TRAVERSE PAR EXAMPLE
  import cats.syntax.parallel._
  val parTraversed: IO[List[Int]] = someWorkload parTraverse someHeavyComputationIO


  // EXERCISE
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(item => item)

  def sequenceGen[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(item => item) // how to deal with implicit value in type(!!)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs parTraverse (item => item)

  def parSequenceGen[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    listOfIOs parTraverse (item => item)

  // EXISTING APPROACH FOR EXERCISE TASKS

  val items: List[IO[Int]] = someWorkloadIO
  // to get IO[List[int]}
  val v1: IO[List[Int]] = parSequence(items)
  val v2: IO[List[Int]] = items.parSequence


  override def run: IO[Unit] = {
    parTraversed.debug.void
  }
}
