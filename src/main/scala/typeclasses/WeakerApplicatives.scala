package typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  trait An_Apply[F[_]] extends Functor[F] with Semigroupal[F] {
    def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)((a: A) => (b: B) => (a, b)))(fb)
    // TODO: implement
    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
      map(product(tuple._1, tuple._2)) {case (a, b) => f(a, b)}
  }
  trait An_Applicative[F[_]] extends An_Apply[F] {
    def pure[A](a: A): F[A]
  }

  import cats.Apply
  import cats.instances.option._ // Apply[Option]
  val applyOption = Apply[Option]
  // apply the function +1 on Some(2)
  val some3 = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some(Some(1),Some(2),Some(3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  def main(args: Array[String]): Unit = {
    println(some3)
    println(optionOfTuple)
    println(sumOption)
  }
}