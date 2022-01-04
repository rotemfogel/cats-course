package part1intro

// @formatter:off
object CatsIntro {

  // this is a valid expression that always produces false - will issue compiler warning
  // val aComparison: Boolean = 2 == "2"

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._

  // part 3 - use type class API
  val intEquality: Eq[Int] = Eq[Int]
  val typeSafeComparison: Boolean = intEquality.eqv(2, 3) // false
  // val unsafeComparison = intEquality.eqv(2, "2") - won't compile

  // part 4 - extension methods
  import cats.syntax.eq._
  val anotherTypeSafeComparison: Boolean = 2 === 3 // false
  val notEqualsComparison: Boolean = 2 =!= 3 // true
  //  val unsafeComparison = 2 === "2"  - won't compile

  // part 5 - extending type class operations to composite types (e.g. list)
  // older cats versions should:
  // import cats.instances.list._
  val listComparison: Boolean = List(1) === List(2) // false

  // part 6 - create a type classes for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) => c1.price == c2.price }

  val compareToyCar: Boolean = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // true

  def main(args: Array[String]): Unit = {
    println(typeSafeComparison)
    println(notEqualsComparison)
    println(listComparison)
    println(compareToyCar)
  }
}
// @formatter:on