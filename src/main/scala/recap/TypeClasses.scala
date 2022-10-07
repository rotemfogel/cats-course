package recap

object TypeClasses {
  case class Person(name: String, age: Int)

  trait JSONSerializer[T] {
    def toJson(t: T): String
  }

  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(t: String): String = s"\"$t\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(t: Int): String = t.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(t: Person): String = s"{\"name\": \"${t.name}\",\"age\": ${t.age}}"
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    list.map((e: T) => serializer.toJson(e)).mkString("[", ",", "]")
  }

  object JSONSyntax {
    implicit class JsonSerializable[T](t: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = {
        serializer.toJson(t)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(listToJson(List(Person("Alice", 23), Person("Bob", 35))))
    println(listToJson(List("Apple", "Banana", "Orange")))
    println(listToJson(List(1, 2, 3)))
    import JSONSyntax._
    println(Person("Bob", 35).toJson)
    println(35.toJson)
    println("Apple".toJson)
  }
}