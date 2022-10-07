package recap

object Implicits {
  case class Person(name: String)

  case class Student(id: Int, name: String, major: String)

  trait JsonSerializer[T] {
    def toJson(t: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map((v: T) => serializer.toJson(v)).mkString("[", ",", "]")

  // implicit val personSerializer: JsonSerializer[Person] = (t: Person) => s"""{"name":"${t.name}"}"""

  implicit def caseClassSerializer[T <: Product]: JsonSerializer[T] = (t: T) => {
    Range.inclusive(0, t.productArity - 1)
      .map((i: Int) => s""""${t.productElementName(i)}": "${t.productElement(i)}"""")
      .mkString("{", ",", "}")
  }

  def main(args: Array[String]): Unit = {
    println(listToJson(List(Person("Alice"), Person("Bob"))))
    println(listToJson(List(Student(1, "Alice", "CS"), Student(2, "Bob", "Sociology"))))
  }
}