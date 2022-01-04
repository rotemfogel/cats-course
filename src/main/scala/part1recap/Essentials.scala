package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions
  val anIfExpression: String = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  val theUnit: Unit = println("hello scala")

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("crunch")
  }

  // singleton
  object MySingleton

  // companion
  object Carnivore // companion object of trait Carnivore

  // generics
  class MyList[A]

  // method notation
  val three: Int = 1 + 2
  val anotherThree: Int = 1.+(2)

  // functional programming
  val incrementor: Int => Int = x => x + 1
  val incremented: Int = incrementor(45) // 46

  // map, flatMap, filter
  val processedList: List[Int] = List(1, 2, 3).map(incrementor) // List(2,3,4)
  val anotherList: List[Int] = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1,2, 2,3, 3,4)

  // for comprehension
  val checkerBoard: List[(Int, Char)] = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val anotherCheckerBoard: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // options and try
  val anOption: Option[Int] = Option(/* something that might be null*/ 3) // Some(3) / None
  val doubleOption: Option[Int] = anOption.map(_ * 2) // Some(6)

  val attempt: Try[Int] = Try(/* something that might throw*/ 42) // Success(42) / Failure(ex)
  val anAttempt: Try[Int] = attempt.map(_ + 10) // Success(52)

  // pattern matching
  val unknown: Any = 45
  val ordinal: String = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }
  unknown match {
    case Some(value) => println(s"the option is not empty [$value]")
    case None => println("the option is empty")
  }

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(8)
  )

  val aFuture: Future[Int] = Future {
    42
  }
  aFuture.onComplete {
    case Success(value) => println(value)
    case Failure(ex) => println(s"failed $ex")
  }

  val improvedFuture: Future[Int] = aFuture.map(_ + 1) // Future(43) when it is completed

  // partial function
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 100
    case 8 => 53
    case 22 => 999
  }

  // higher kinded types
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {
  }
}
