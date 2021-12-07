package part1Intro

import cats.Monoid

object CatsTypeClass extends App {

  // FUNCTOR
  trait MyFunctor[F[_]] {
    def map[A, B](initiaalValue: F[A])(f: A => B): F[B]
  }
  import cats.Functor
  import cats.instances.list._
  val listFunctor = Functor[List]

  def increment[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor._
  def increment2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  // APPLICATIVE
  // ability wrap types
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val list: List[Int] = applicativeList.pure(43)

  import cats.syntax.applicative._
  val list2: List[Int] = 43.pure[List]

  // FLATMAP
  // chain multiple combinations
  trait MyFlatMap[F[_]]  extends MyFunctor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]

  import cats.syntax.flatMap._
  def crossProduct[F[_] : FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap( a => fb.map( b => (a, b) ))

  // MONAD
  // applicative + flatMap
  trait MuMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList = Monad[List]
  def crosProdWithMonad[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A,B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)

    // Functor   ->     FlatMap
    //           ->     Applicative    ->   Monad
  }

  // ERROR LIKE TYPE CLASSES
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val errorEither = ApplicativeError[ErrorOr, String]

  val desired: ErrorOr[Int] = errorEither.pure(42)
  val failed:  ErrorOr[Int] = errorEither.raiseError("Some error")

  import cats.syntax.applicativeError._
  val failed2: ErrorOr[Int] = "Error Message".raiseError[ErrorOr, Int]

  trait MyMonaderror[F[_], E] extends MyApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monaderroreither = MonadError[ErrorOr, String]


  // TRAVERSE
  // turn nested wraps inside out. List(Some(1), Some(2), Some(3)) => Some(List(1,2,3))
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }
  val optionsList: List[Option[Int]] = List(Some(1), Some(43), None)

  import cats.Traverse
  val listTraverse = Traverse[List]
  val result: Option[List[Int]] = listTraverse.traverse(List(1,2,3))(x => Option(x))
  println {
    result
  }
  import cats.syntax.traverse._
  val result2: Option[List[Int]] = List(1,2,3).traverse(x => Option(x))
}
