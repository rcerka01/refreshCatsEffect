package part2effects

import scala.io.StdIn

case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] =
    MyIO ( () => f ( unsafeRun() ) )
  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO ( () => f ( unsafeRun () ).unsafeRun () )
}

object Effects extends App {

  val anIO: MyIO[Int] = MyIO { () =>
    println("Hallo")
    42
  }
  val x = anIO.unsafeRun()
  println(x)

  // EXERCISES

  // 1. current time in ms
  val clock: MyIO[Long] = MyIO { () =>
    System.currentTimeMillis()
  }
  println {
    clock.unsafeRun()
  }

  // 2. measure execution time
  def measure[A](comp: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
            - <- comp
      endTime <- clock
  } yield ( endTime - startTime)

  println {
    measure(MyIO( () => Thread.sleep(500))).unsafeRun()
  }

  // 3. print to console
  def printToConsole(txt: String) = MyIO { () =>
    println(txt)
  }

  // 4. read from console
  def read = MyIO { () =>
    StdIn.readLine()
  }

  // test
  def runme() = {
    val program =
      for {
        line1 <- read
        line2 <- read
            - <- printToConsole(line1 + line2)
      } yield ()
    program.unsafeRun()
  }
  runme()
}
