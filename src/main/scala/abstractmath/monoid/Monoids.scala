package abstractmath.monoid

object Monoids {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.semigroup._

  def main(args: Array[String]): Unit = {
    // the combine (|+|) method is associative
    val numbers: List[Int] = (1 to 1000).toList
    val sumLeft: Int = numbers.foldLeft(0)((_: Int) |+| (_: Int))
    val sumRight: Int = numbers.foldRight(0)((_: Int) |+| (_: Int))

    // A Monoid is a Semigroup with an identity value
    val intMonoid: Monoid[Int] = Monoid[Int]
    assert(intMonoid.empty == 0)

    val stringMonoid: Monoid[String] = Monoid[String]
    assert(stringMonoid.empty == "")

    import cats.instances.option._ // construct a Monoid[Option[Int]]
    val emptyOption: Option[Int] = Monoid[Option[Int]].empty // default is None
    assert(emptyOption.isEmpty)
    val combinedOptions: Option[Int] = Monoid[Option[Int]].combine(Option(2), emptyOption)
    assert(combinedOptions.contains(2))

    // general API
    def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)((_: T) |+| (_: T))

    assert(sumLeft == sumRight)
    assert(combineFold(numbers) == sumLeft)

    val phonebooks: List[Map[String, Int]] =
      List(
        Map(
          "Alice" -> 123,
          "Bob" -> 456,
        ),
        Map(
          "Charlie" -> 789,
          "Daniel" -> 1011
        ),
        Map(
          "Eli" -> 1213
        )
      )

    import cats.instances.map._
    //    implicit val phonebookMonoid: Monoid[Map[String, Int]] = new Monoid[Map[String, Int]] {
    //      override def empty: Map[String, Int] = Map.empty[String, Int]
    //      override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] = x ++ y
    //    }
    println(combineFold(phonebooks))

    case class Item(name: String, amount: Double)
    case class ShoppingCart(items: List[Item], total: Double) {
      def +(other: ShoppingCart): ShoppingCart = ShoppingCart(this.items ++ other.items, this.total + other.total)
    }
    object ShoppingCart {
      def empty: ShoppingCart = ShoppingCart(List(), 0)

      def apply(item: Item): ShoppingCart = ShoppingCart(List(item), item.amount)

      def apply(items: List[Item]): ShoppingCart = {
        ShoppingCart(items, items.map((_: Item).amount).sum)
      }
    }
    val shoppingCarts: List[ShoppingCart] = List(
      ShoppingCart(List(Item("item1", 9.99), Item("item2", 5.99))),
      ShoppingCart(List(Item("item3", 19.99), Item("item4", 9.99))),
      ShoppingCart.empty
    )

    implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
      ShoppingCart.empty,
      (x: ShoppingCart, y: ShoppingCart) => x + y
    )

    def checkout(carts: List[ShoppingCart]): ShoppingCart = combineFold(carts)

    println(checkout(shoppingCarts))
  }
}