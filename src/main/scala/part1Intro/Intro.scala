package part1Intro

import cats.effect.{IO, IOApp}

object Intro extends IOApp.Simple {
  override def run: IO[Unit] =
    IO.println("It works")
}
