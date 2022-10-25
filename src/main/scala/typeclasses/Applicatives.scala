package typeclasses

object Applicatives {
  // an extension of Functor that implement the pure method (Monad)
  // useful as Functor with pure method
  import cats.Applicative
  import cats.instances.list._

  val listApplicative: Applicative[List] = Applicative[List]
  val list: List[Int] = listApplicative.pure(42) // List(42)

  import cats.instances.option._
  val optionApplicative: Applicative[Option] = Applicative[Option]
  val option: Option[Int] = optionApplicative.pure(42) // Some(42)

  // pure extension method
  import cats.syntax.applicative._
  val pureList: List[Int] = 2.pure[List]
  val pureOption: Option[Int] = 2.pure[Option]

  // Applicative extends Functor (map + pure)
  // Monad extends Applicative ((map + pure) + flatMap)
  // a special case is Validated where valid is a wrapper much like pure
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validValue: ErrorsOr[Int] = Validated.valid(42)
  val modifiedValue: ErrorsOr[Int] = validValue.map((_: Int) + 1) // candidate for Applicative

  // this is similar to
  val validateApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]
  validateApplicative.pure(Validated.valid(1))

  // TODO: thought experiment
  def ap[F[_], B, T](f: F[B => T])(fb: F[B]): F[T] = ???
  def product[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
    ap(applicative.map(fa)((a: A) => (b: B) => (a, b)))(fb)
    // or applicative.ap(applicative.map(fa)(a => (b: B) => (a, b)))(fb)
    // or applicative.product(fa, fb)
  }
  // an Applicative can be a Semigroupal (define a product method) given an
  // ap method -> Applicative can extend Semigroupal

  def main(args: Array[String]): Unit = {
    println(list)
    println(modifiedValue)
  }
}