package abstractmath.semgroup

object Semigroups {

  /** semigroups combine elements of the same type */

  import cats.Semigroup
  import cats.instances.int._

  val intSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombine: Int = intSemigroup.combine(2, 4) // default combine method is addition

  import cats.instances.string._

  val stringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombine: String = stringSemigroup.combine("Hello ", "World!") // default combine method is concat

  // specific API
  def reduceList(list: List[Int]): Int = list.reduce(intSemigroup.combine)

  def reduceList(list: List[String]): String = list.reduce(stringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance[Expense]((x: Expense, y: Expense) => Expense(math.max(x.id, y.id), x.amount + y.amount))

  // extension methods from Semigroup - |+|

  import cats.syntax.semigroup._

  val combinedInt: Int = 1 |+| 2 // combine will require an implicit Semigroup[Int]
  val concatString: String = "combined " |+| "String"

  // shorter version with combine operator
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce((_: T) |+| (_: T))

  val ex1: Expense = Expense(1, 9.99)
  val ex2: Expense = Expense(2, 19.99)
  val combinedExpense: Expense = ex1 |+| ex2

  def main(args: Array[String]): Unit = {
    println(intCombine)
    println(stringCombine)
    val numbers: List[Int] = Range.inclusive(1, 10).toList
    val fruit: List[String] = List("apple ", "banana ", "carrot")
    println(reduceList(numbers))
    println(reduceList(fruit))
    // general API
    println(reduceThings(numbers))
    println(reduceThings(fruit))
    import cats.instances.option._
    println(reduceThings(numbers.map((e: Int) => Option(e))))
    println(reduceThings(fruit.map((e: String) => Option(e))))
    val expenseList: List[Expense] = List(ex1, ex2)
    println(reduceThings(expenseList))
    println(reduceThings2(expenseList))
    println(combinedInt)
    println(concatString)
    println(combinedExpense)
  }
}