package intro

object CatsIntro {

  case class ToyCar(model: String, price: Double)

  def main(args: Array[String]): Unit = {
    // Eq
    import cats.Eq
    import cats.implicits.catsSyntaxEq
    import cats.instances.int._

    val intEqual: Eq[Int] = Eq[Int]
    assert(!intEqual.eqv(2, 3))
    assert(2 === 2)
    assert(2 =!= 3)

    import cats.instances.list._
    assert(List(1) === List(1))
    assert(List("1") =!= List("2"))

    implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]((c1: ToyCar, c2: ToyCar) => c1.price === c2.price)
    val toyCar1: ToyCar = ToyCar("Ferrari", 38.2)
    val toyCar2: ToyCar = ToyCar("Lamborghini", 38.2)
    assert(toyCar1 === toyCar2)
  }
}