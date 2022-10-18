package data

//noinspection ScalaUnusedSymbol,ScalaDocUnknownParameter
object FunctionalState {
  /**
   * A data structure representing a state and the result of the computation on it (=>)
   *
   * @tparam S - State
   * @tparam A - value we obtain from a result of computation
   */
  type A_State[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State((c: Int) => (c + 1, s"added 1 to $c"))

  /* // example of BAD iterative code
    var a: Int = 10
    a += 1 // 11
    println(s"added 1 to 10, got $a")
    a *= 5 // 55
    println(s"multiplied by 5, got $a")
   */

  // iterative code using State and FP
  val addOne: State[Int, String] = State((s: Int) => (s + 1, s"added 1 to $s, got ${s + 1}"))
  val multiplyByFive: State[Int, String] = State((s: Int) => (s * 5, s"multiplied $s by 5, got ${s * 5}"))
  val composite: State[Int, (State[Int, String], State[Int, String])] =
    for {
      first <- addOne
      second <- multiplyByFive
    } yield (addOne, multiplyByFive)

  // why not chain functions instead of State?
  val f1: Int => (Int, String) = (s: Int) => (s + 1, s"added 1 to $s, got ${s + 1}")
  val f2: Int => (Int, String) = (s: Int) => (s * 5, s"multiplied $s by 5, got ${s * 5}")
  val compositeF: Int => (String, (Int, String)) = f1.andThen {
    case (newState, log) => (log, f2(newState))
  } // .andThen { case (v1, v2, v3).... }

  // TODO: an online store that sells guitars
  case class ShoppingCart(items: List[(String, Double)], amount: Double)
  object ShoppingCart {
    def empty: ShoppingCart = ShoppingCart(List(), 0.0)
  }
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State { cart: ShoppingCart =>
      (ShoppingCart((item, price) :: cart.items, cart.amount + price), cart.amount + price)
    }

  val myCart: State[ShoppingCart, Double] =
    for {
      _ <- addToCart("Fender Stratocaster Ultimate", 2199)
      _ <- addToCart("Elixir Strings", 19)
      total <- addToCart("Electric Cable", 8)
    } yield total

  // TODO: mental gymnastics
  /**
   * return a State data structure that, when run,
   * will not change the state, but will issue the value f(A)
   * @param f - function
   * @tparam A - A
   * @tparam B - B
   * @return - f(A)
   */
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))

  /**
   * return a State data structure that, when run,
   * returns the value of that state and makes no change
   * @tparam A - A
   * @return - State[A. A]
   */
  def get[A]: State[A, A] = State((a: A) => (a, a))

  /**
   * return a State data structure that, when run,
   * returns a Unit and sets the value of that state to a
   * @param a - A
   * @tparam A - A
   * @return - State[A, Unit]
   */
  def set[A](a: A): State[A, Unit] = State((_: A) => (a, ()))

  /**
   * return a State data structure that, when run,
   * returns a Unit and sets the value of that state to f(A)
   * @param f - function
   * @tparam A - A
   * @return - State[A, Unit]
   */
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), ()))

  // all the above methods are implemented in cats.data.State._ object

  def main(args: Array[String]): Unit = {
    println(countAndSay.run(1).value)
    println("-" * 30)
    println(composite.run(10).value)
    println("-" * 30)
    // the final result will be nested within the tuple
    println(compositeF(10))
    println("-" * 30)
    println(myCart.run(ShoppingCart.empty).value)
    assert(modify[Int]((_: Int) + 1).run(1).value._1 == 2)
    assert(set[Int](1).run(1).value._1 == 1)
    val getResult: (Int, Int) = get[Int].run(1).value
    assert(getResult._1 == getResult._2 && getResult._1 == 1)
    val inspectResult: (Int, String) = inspect[Int, String]((_: Int).toString).run(10).value
    assert(inspectResult._1 == 10)
    assert(inspectResult._2 == "10")

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 10)
      b <- get[Int]
      _ <- modify[Int]((_: Int) + 43)
      c <- inspect[Int, Int]((_: Int) * 2)
    } yield (a, b, c)
    println(program.run(0).value)

  }
}