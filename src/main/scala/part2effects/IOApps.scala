package part2effects

import cats.effect.{ExitCode, IO, IOApp}

class IOApps

// WITH ARGS:
object MyObject1 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = ???
}

// SIMPLE
object MyObject2 extends  IOApp.Simple {
  override def run: IO[Unit] = ???
}
