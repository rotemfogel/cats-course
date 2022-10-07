package intro

object TypeClassVariance {

  import cats.implicits.catsSyntaxEq
  import cats.instances.int._
  import cats.instances.option._

  val validComparison: Boolean = Option(1) === Option(0)
  // val invalidComparison = Some(1) === None // Eq[Some[Int]]
  // although Some <: Option, Eq[Some[Int]] is not <: Eq[Option[Int]]
  val validComparison2: Boolean = Option(1) =!= Option.empty[Int]

  class Animal

  class Cat extends Animal

  // covariance - a generic type annotation which allows propagating subtyping to the generic type
  // covariance - class has a T
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal => Cage[Cat] <: Cat[Animal]

  // contravariance - a generic type annotation which allows back-propagating subtyping to the generic type
  // contravariance - class acts on T
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // cat <: Animal => Vet[Animal] <: Vet[Cat]

  // variance affects how type classes are fetched
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Arghh")

  makeSound[Animal] // type class instance defined above
  makeSound[Cat] // also ok. type class instance Animal applies for Cat

  /** rules:
    *   1. contravariant type classes can use superclass instance if nothing is available strictly for that type
    *
    * 2. covariant type classes will always use the more specific type class instance for that type, but may confuse the
    * compiler, if the general type class is also present
    *
    * 3. You cant have both :(
    */
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "general"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "cat"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  println(organizeShow[Cat]) // the compiler will inject CatsShow type class instance
  // println(organizeShow[Animal])
  // will not compile, since the compiler sees two valid implementations (GeneralAnimalShow ad CatsShow)

  def main(args: Array[String]): Unit = {}
}