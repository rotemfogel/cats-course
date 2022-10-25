package typeclasses

import cats.{Applicative, Apply}

import scala.annotation.unused

object WeakerMonads {

  trait A_FlatMap[F[_]] extends Apply[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def ap[A, B](f: F[A => B])(fa: F[A]): F[B] =
      flatMap(fa)((a: A) => map(f)((fn: A => B) => fn(a)))
  }
  @unused
  trait A_Monad[F[_]] extends A_FlatMap[F] with Applicative[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)((a: A) => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // map extension method

  // same as Monad implementation
  def getPairs[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)
}