package part1intro

import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._

// @formatter:off
object TypeClassVariance {
  val aComparison: Boolean = Option(2) === Option(3)
  // Some(2) is an instance of Some[Int], so it cannot construct Eq[Some[Int]]
  // although Some <: Option
  // val anInvalidComparison = Some(2) === None

  // variance
  class Animal
  class Cat extends Animal

  /**
   * rule of thumb:
   * if a class has [T] -> covariant
   * if a class acts/operates on [T] -> contravariant
   */
  // covariant Type - subtyping is propagated to the generic type
  class Cage[+T]
  val catCage = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type - subtyping is propagated backwards the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // if Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // contravariant type class
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  // api
  def makeSound[T](implicit soundMaker: SoundMaker[T]): String = "wow" // implementation not important
  val makeAnimalSound: String = makeSound[Animal] // ok - type class instance defined above (AnimalSoundMaker)
  val makeCatSound: String = makeSound[Cat] // ok - type class instance for Animal is also applicable for Cat

  /**
   * rule #1:
   * contravariant type classes can use superclass instances if nothing is available
   * for that type
   */

  // using contravariance, we can solve the problem of Some(2) === None
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  val optionSound: String = makeSound[Option[Int]]
  val someSound: String = makeSound[Some[Int]]

  // covariant type class
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    def show: String = "animal show time"
  }
  implicit object CatShow extends AnimalShow[Cat] {
    def show: String = "cat show time"
  }

  // part 3 - api
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  /**
   * rule #2:
   * contravariant type classes will always use the most specific type class instance
   * for that type, but may confuse the compiler if the general type class instance
   * is also present
   */

  /**
   * rule #2:
   * you can't have both (covariance/contravariance)
   * Cats uses invariant type classes
   */
  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])  // ok - compiler will inject CatShow as implicit
    // println(organizeShow[Animal]) // won't compile => ambiguity - compiler sees 2 potential instances (Animal, Cat)
  }
}
// @formatter:on
