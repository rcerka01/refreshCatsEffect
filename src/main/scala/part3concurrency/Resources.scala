package part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  def openFileScaner(path: String): IO[Scanner] =
    IO( new Scanner( new FileReader( new File( path ) ) ) )

  // PROBLEM
  // brackets, if nested, can be hard to read, e.g:
  def conFromConFromConf(path: String): IO[String] =
    openFileScaner(path)
      .bracket { scanner =>
        IO(new Connection(scanner.nextLine()))
          .bracket { con =>
            con.open() >> IO.never
          } (con => con.close().void)
      } (scan => IO("close file").debug >> IO(scan.close()) )

  // RESOURCES
  val conResource: Resource[IO, Connection] = Resource.make( IO(new Connection("localhost")) ) (con => con.close().void)

  val res: IO[Unit] = for {
    fib <- conResource.use(con => con.open() >> IO.never).start
      _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()

  // RESOURCES VS BRACKETS
  val resource: IO[String] = IO("resource")
  val use: String => IO[String] =
    string => IO("Using string").debug
  val release: String => IO[Unit] =
    string => IO("Release resource").debug.void

  val bracket: IO[String] = resource.bracket(use)(release)
  val resour:  IO[String] = Resource.make(resource)(release).use(use)

  // EXERCISE
  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFileGiven(path: String): IO[Unit] =
    Resource.make(  IO(s"opening file at $path") >> openFileScaner(path) ) ( scanner => IO(s"closing file at $path").debug >> IO(scanner.close()) )
      .use(scanner => readLineByLine(scanner))

  def cancelReadFile(path: String): IO[Unit] =
    for {
      fib <- bracketReadFileGiven(path).start
        _ <- IO.sleep(2.seconds) >> fib.cancel
    } yield ()

  // NESTED RESOURCES
  // flatMap
  def nestedresourceFlatMap(path: String): Resource[IO, Connection] =
    Resource.make( IO("Opening file").debug >> openFileScaner(path)) (sc => IO("Close file").debug >> IO(sc.close()))
      .flatMap(sc => Resource.make( IO(new Connection(sc.nextLine()))) (c => c.close().void) )
  // for
  def nestedresourceFor(path: String): Resource[IO, Connection] =
    for {
      scanner <- Resource.make( IO("Opening file").debug >> openFileScaner(path)) (sc => IO("Close file").debug >> IO(sc.close()))
      connect <- Resource.make( IO(new Connection(scanner.nextLine()))) (c => c.close().void)
    } yield (connect)

  def makeConnection: IO[Unit] = nestedresourceFor("build.sbt").use(c => c.open() >> IO.sleep(10.seconds))

  def cancelConnectionAfterOneSecond: IO[Unit] =
    for {
      fib <- makeConnection.start
        _ <- IO.sleep(1.seconds) >> IO("--Cancelling--").debug >> fib.cancel
    } yield ()

  // FINALIZERS
  val withFinalizer: IO[String] = IO("Working").debug.guarantee(IO("Finalized").debug.void)

  val withFinalizeForEachState: IO[String] = IO("Working").debug.guaranteeCase {
    case Succeeded(fa) => fa flatMap(result => IO(s"Finalized $result").debug.void)
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException("Cancelled"))
  }



  override def run: IO[Unit] = withFinalizeForEachState.void
}
