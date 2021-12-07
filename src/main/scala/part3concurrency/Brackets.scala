package part3concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt

object Brackets extends IOApp.Simple {

  // PROBLEM
  class Connection(url: String) {
    def openConnection(): IO[String] = IO("Opening connection").debug
    def closeConnection(): IO[String] = IO("Closing connection").debug
  }

  val canLeakResurses: IO[Unit] =
    for {
      fib <- ( new Connection("localhost").openConnection() >> IO.sleep( (Int.MaxValue).seconds ) ).start
      _ <- IO.sleep(500.millisecond) *> fib.cancel
    } yield ()

  // FIX RESOURCE LEAKING PROBLEM
  // with void onCancel event
  val fixResourceLeaking: IO[Unit] =
    for {
      con <- IO(new Connection("localhost"))
      fib <- con.openConnection() >> IO.sleep( (Int.MaxValue).seconds ).onCancel( con.closeConnection().void ).start
      _ <- IO.sleep(500.millisecond) >> fib.cancel
    } yield ()

  // NICER FIX WITH BRACKET
  val bracket: IO[Unit] = IO(new Connection("localhost")).bracket (
    use =  con => con.openConnection() >> IO.sleep((Int.MaxValue).seconds) )(
    release = con => con.closeConnection().void )

  val fixResourceLeakingWithBracket: IO[Unit] =
    for {
      fib <- bracket.start
      _ <- IO.sleep(1.second) *> fib.cancel
    } yield ()

  // EXERCISE (my solution, flaky)
  def openFileScaner(path: String): IO[Scanner] =
    IO( new Scanner( new FileReader( new File( path ) ) ) )

  def bracketReadFileMy(path: String): IO[Unit] = {
    val scanner = openFileScaner(path)

    @tailrec
    def aux(isc: Scanner): Unit = {
      println( isc.nextLine() )
      Thread.sleep(500)
      aux(isc)
    }

    val exBracket = scanner.bracket( sc => IO(aux(sc)).debug) (sc => IO(sc.close()))

    for {
      fib <- exBracket.debug.start
        _ <- IO.sleep(2.seconds) >> fib.cancel
    } yield ()
  }

  // EXERCISE (given)
  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFileGiven(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      openFileScaner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug >> IO(scanner.close())
      }


  override def run: IO[Unit] =
    bracketReadFileGiven("build.sbt").debug.void
}
