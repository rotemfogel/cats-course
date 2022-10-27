package typeclasses

import cats.{Eval, Monoid}

object Folding {

  object ListExcersizes {
    // TODO: Implement the following methods using foldLeft/foldRight
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, b) => f(a) :: b)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((a, b) => a ++ f(b))
      // can be expressed with a foldRight as well
      // list.foldLeft(List.empty[B])((a, b) => a.foldRight(f(b))(_ :: _))
    def filter[A](list: List[A])(f: A => Boolean): List[A] =
        list.foldRight(List.empty[A])((a, b) => if (f(a)) a :: b else b)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  val list: List[Int] = (1 to 10).toList

  // Foldable is very good for generalizing fold operations on different types
  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(list, 0)(_ + _) // 55
  import cats.instances.option._ // implicit Foldable[Option]
  val optionSum: Int = Foldable[Option].foldLeft(Option(2), 40)(_ + _) // 42
  // foldRight can be implemented in stack recursive way
  // using Eval will make it stack safe, since chaining Evals is tail recursive
  val sumRight: Eval[Int] = Foldable[List].foldRight(list, Eval.now(0))((n, e) => e.map(_ + n)) // Eval(55)
  // using monoid
  import cats.instances.int._ // implicit Monoid[Int]
  val monoidSum: Int = Foldable[List].combineAll(list)
  val mappedConcat: String = Foldable[List].foldMap(list)(_.toString) // requires implicit Monoid[String]

  def main(args: Array[String]): Unit = {
    import ListExcersizes._
    println(map(list)(_ + 1))
    println(flatMap(list)(a => if (a % 2 == 0) List(-a) else List.empty[Int]))
    println(filter(list)(_ %2 == 0))
    println(combineAll(list))
    println("-" * 30)
    println(sum)
    println(optionSum)
    println(sumRight.value)
    println("-" * 30)
    println(monoidSum)
    println(mappedConcat)
  }
}