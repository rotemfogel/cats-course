package abstractmath.functor

import scala.util.Try

//noinspection ScalaUnusedSymbol
object Functors {
  // map functions exist everywhere
  val incrementedList: List[Int] = List(1, 2, 3).map((_: Int) + 1) // List(2,3,4)
  val incrementedOption: Option[Int] = Option(1).map((_: Int) + 1) // Option(3)
  val incrementedTry: Try[Int] = Try(42).map((_: Int) + 1) // Success(43)

  //noinspection ScalaUnusedSymbol
  // a Functor is a generic mapping between categories
  // simplified version:
  trait A_Functor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor: Functor[List] = Functor[List]
  val incremented: List[Int] = listFunctor.map(List(1, 2, 3))((_: Int) + 1) // List(2,3,4)

  import cats.instances.option._

  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOptions: Option[Int] = optionFunctor.map(Option(1))((_: Int) + 1) // Some(2)

  import cats.instances.try_._

  val tryFunctor: Functor[Try] = Functor[Try]
  val incrementedTryF: Try[Int] = tryFunctor.map(Try(42))((_: Int) + 1) // Success(43)

  // Functors are used to generalize an API

  /**
   * instead of 3 different methods:
   * def do10x(list: List[Int]): List[Int] = list.map(_ * 10)
   * def do10x(option: Option[Int]): Option[Int] = option.map(_ * 10)
   * def do10x(t: Try[Int]): Try[Int] = t.map(_ * 10)
   *
   * Generalize the do10x:
   */
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)((_: Int) * 10)

  // using extension method for Functor (map)

  import cats.syntax.functor._

  // [F[_]: Functor] - there is an implicit Functor[F] in the scope
  def do10xShort[F[_] : Functor](container: F[Int]): F[Int] = container.map((_: Int) * 10)

  sealed trait Tree[+T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[+T](value: T) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      // currently stack recursive
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  /*
  create a tree
        1
       / \
      2   3
     / \ / \
    4  5 6  7
   */
  val tree: Tree[Int] =
    Branch(1,
      Branch(2, Leaf(4), Leaf(5)),
      Branch(3, Leaf(6), Leaf(7))
    )

  def main(args: Array[String]): Unit = {
    println(incremented)
    println(incrementedOptions)
    println(incrementedTryF)
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(1)))
    println(do10x(Try(42)))
    println(do10x(tree))
    println(do10xShort(tree))
    // simply using FunctorOps (syntax)
    print(tree.map((_: Int) * 10))
  }
}