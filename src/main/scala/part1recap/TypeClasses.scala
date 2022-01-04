package part1recap

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JsonSerializer[T] {
    def toJson(t: T): String
  }

  // part 2 - implicit type class instances
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(t: String) = s""""$t""""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(t: Int): String = t.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(t: Person): String =
      s"""{"name": "${t.name}", "age": ${t.age}}"""
  }

  // part 3 - offer some API
  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(e => serializer.toJson(e)).mkString("[", ",", "]")

  // part 4 - extending the existing types with extension methods
  object JSONSyntax {
    implicit class JsonSerializable[T](value: T)(implicit serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    val bob = Person("Bob", 45)
    println(listToJson(List(Person("Alice", 23), bob)))
    println(listToJson(List("a", "b", "c")))
    println(listToJson(List(1, 2, 3)))
    import JSONSyntax._
    println(bob.toJson)
  }
}
