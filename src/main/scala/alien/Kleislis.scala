package alien

import cats.data.Reader

object Kleislis {
  /**
   * A Kleisli is a generic data structure
   * that will help with composing functions that returning wrapper instances
   */

  // function composition
  val PlainFn1: Int => String = x => s"$x is ${if (x % 2 == 0) "even" else "odd"}"
  val plainFn2: Int => Int = x => x * 3
  val plainFunc3 = plainFn2.andThen(PlainFn1)

  // we want to compose the following 2 functions
  val fn1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val fn2: Int => Option[Int] = x => Some(x * 3)

  // we cannot compose fn3 => fn2 andThen fn1, since the functions return an option
  // fn2.andThen(f1) will fail, since we will need to wrap the returned value from f2 before
  // sending it to f1

  import cats.data.Kleisli
  // Kleisli[Option, Int, String] ==> Kleisli will Wrap a function from Int => String with an Option (higher kinded type)
  // since it's declared as Kleisli[F[_], -A, B]
  val kleisliFn1: Kleisli[Option, Int, String] = Kleisli(fn1)
  val kleisliFn2: Kleisli[Option, Int, Int] = Kleisli(fn2)
  // now we can compose them
  val kleisliFn3: Kleisli[Option, Int, String] = kleisliFn2.andThen(kleisliFn1)

  val multiply = kleisliFn2.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain = kleisliFn2.flatMap(_ => kleisliFn1)

  // TODO:
  import cats.Id // fake wrapper for normal type Id[A] = A
  val times2 = Kleisli[Id, Int, Int](_ * 2)
  val plus4 = Kleisli[Id, Int, Int](_ + 4)
  val composed = times2.flatMap(t2 => plus4.map(_ + t2))
  // equals
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  // Kleisli behaves the same as Reader, so If we replace Kleisli with Reader...
  val rTimes2 = Reader[Int, Int](_ * 2)
  val rPlus4 = Reader[Int, Int](_ + 4)
  val rComposed = for {
    t2 <- rTimes2
    p4 <- rPlus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(kleisliFn3(2))  // Some("6 is even")
    println(kleisliFn3(4))  // Some("12 is even")
    println(kleisliFn3(3))  // None
    println(multiply(3))    // 18 = (3 * 3) * 2
    println(chain(2))       // Some("2 is even")
    println("-" * 30)       // Some("2 is even")
    println(composed(3))    // 13
    println(composedFor(3)) // 13
    println(rComposed(3))   // 13

  }
}