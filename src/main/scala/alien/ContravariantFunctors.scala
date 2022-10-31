package alien

import cats.Monoid

object ContravariantFunctors {

  trait Format[T] { self => // contravariant
    def format(a: T): String
    def contramap[A](f: A => T): Format[A] = (a: A) => self.format(f(a))
  }

  def format[A](a: A)(implicit f: Format[A]) = f.format(a)

  implicit object StringFormat extends Format[String] {
    override def format(a: String): String = s""""$a""""
  }
  implicit object IntFormat extends Format[Int] {
    override def format(a: Int): String = a.toString
  }
  implicit object BooleanFormat extends Format[Boolean] {
    override def format(a: Boolean): String = if (a) "True" else "False"
  }

  // problem: Given Format[MyType], can we have Format[Option[MyType]] or Format[List[MyType]]
  //          Can we have an automatic wrapper given an implicit Format[MyType]
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  /**
   * IntFormat:
   * first: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
   * second: Format[Option[Option[Int]]] = first.contramap[Option[Option[Int]]](_.get)
   *
   * basically:
   *   IntFormat
   *     .contramap[Option[Int]](_.get)
   *     .contramap[Option[Option[Int]]](_.get)
   *
   * Because the format function applies the function before formatting (e.g. self.format(f(a))),
   * then it will be executed in reverse order (i.e. format(first.format(second)))
   * so, Option(Option(42)) => Option(42) => 42
   * This is why type classes which manipulates a value and has a contramap function,
   * such as Format[T] are called contravariant
   */

  import cats.instances.int._
  import cats.{Contravariant, Show} // implicit Show[Int]
  val showInt: Show[Int] = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInt)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val shorterShowOption: Show[Option[Int]] = showInt.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("a string"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    println(format(Option(Option(42))))
    println("-" * 30)
    println(showOption.show(None))
    println(showOption.show(Some(11)))
    println(shorterShowOption.show(None))
    println(shorterShowOption.show(Some(11)))
  }
}