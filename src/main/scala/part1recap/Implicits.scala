package part1recap

object Implicits {

  // implicit classes
  implicit class IntExtension(i: Int) {
    def isOdd: Boolean = i % 2 == 0
  }

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class PersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // importing implicit conversion

  import scala.concurrent.duration._

  val oneSecond: FiniteDuration = 1.second

  // implicit arguments and values
  def increment(i: Int)(implicit amount: Int): Int = i + amount

  def multiply(i: Int)(implicit amount: Int): Int = i * amount

  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  implicit val personSerializer: JsonSerializer[Person] = (person: Person) =>
    s"""{"name": "${person.name}"}"""

  // implicit methods
  implicit def caseClassSerializer[T <: Product]: JsonSerializer[T] = (value: T) =>
    value.productIterator.zipWithIndex.map { case (e, i) =>
      s""""${value.productElementName(i)}": "$e""""
    }.mkString("{", ",", "}")

  case class Cat(name: String, color: String)

  def main(args: Array[String]): Unit = {
    println("peter".greet)

    require(!1.isOdd)
    require(2.isOdd)

    implicit val defaultAmount: Int = 10
    require(increment(1) == 11)
    require(multiply(2) == 20)

    val personsJson: String = listToJson(List(Person("Alice"), Person("Bob")))
    println(personsJson)

    val garfield = Cat("Garfield", "Ginger")
    println(caseClassSerializer.toJson(garfield))

    val mitzi = Cat("Mitzi", "Black")
    println(listToJson(List(garfield, mitzi)))
  }
}
