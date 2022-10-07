package abstractmath.monad

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object optionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    @tailrec override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(a)) => tailRecM(a)(f)
        case Some(Right(b)) => Option(b)
      }
  }

  // TODO: define a monad for the identity type
  type Identity[T] = T
  implicit object identityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    @tailrec override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => b
      }
  }

  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](a: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO: define a monad for the Tree
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Leaf(v) => f(v)
        // stack recursive
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      //noinspection ScalaUnusedSymbol(
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = {
        t match {
          case Leaf(Left(a)) => stackRec(f(a))
          case Leaf(Right(b)) => Leaf(b)
          case Branch(l, r) => Branch(stackRec(l), stackRec(r))
        }
        // to use simply call: stackRec(f(a))
      }

      @tailrec def tailRec(in: List[Tree[Either[A, B]]],
                           used: Set[Tree[Either[A, B]]],
                           out: List[Tree[B]]): Tree[B] = {
        if (in.isEmpty) out.head
        else in.head match {
          case Leaf(Left(a)) => tailRec(f(a) :: in.tail, used, out)
          case Leaf(Right(b)) => tailRec(in.tail, used, Leaf(b) :: out)
          case node@Branch(l, r) =>
            if (!used.contains(node)) tailRec(r :: l :: in, used + node, out)
            else tailRec(in.tail, used, Branch(out.head, out.tail.head) :: out.drop(2))
        }
      }
      tailRec(List(f(a)), Set(), List())
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(changedTree)
  }
}