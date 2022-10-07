package abstractmath

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}

object Monads {
  // lists
  val numbersList: List[Int] = List(1, 2, 3)
  val charsList: List[Char] = List('a', 'b', 'c')

  // TODO: create all combinations of (number, character)
  val combinations1: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinations2: List[(Int, Char)] =
    for {
      n <- numbersList
      c <- charsList
    } yield (n, c)


  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')

  // TODO: create a combinations of (number, character)
  val combined1: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combined2: Option[(Int, Char)] =
    for {
      n <- numberOption
      c <- charOption
    } yield (n, c)

  // futures
  val es: ExecutorService = Executors.newFixedThreadPool(2)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
  val numberFuture: Future[Int] = Future(2)
  val charFuture: Future[Char] = Future('d')

  // TODO: create a combinations of (number, character)
  val combinedFuture1: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinedFuture2: Future[(Int, Char)] =
    for {
      n <- numberFuture
      c <- charFuture
    } yield (n, c)

  /**
   * We have 3 different data types (List/Option/Future) which can be combined
   * with the same patterns:
   * 1. wrap a value to an M value (List,Try,Option...)
   * 2. transform M values using flatMap
   *
   * The type class the formalizes this behavior
   * is a Monad
   */
  trait A_Monad[M[_]] {
    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A], f: (A => M[B])): M[B]
  }

  import cats.Monad
  import cats.instances.option._

  val optionMonad = Monad[Option]
  val option = optionMonad.pure(4) // Some(4)
  val transformed = optionMonad.flatMap(option)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._ // Monad[List]

  val listMonad = Monad[List]
  val list: List[Int] = listMonad.pure(1) // List(1)
  val transformedList: List[Int] = listMonad.flatMap(list)(x => List(x, x + 1)) // List(1,2)

  // TODO: add Monad[Future]

  import cats.instances.future._

  val futureMonad = Monad[Future]
  val future: Future[String] = futureMonad.pure("HA") // Future[String]
  val transformedFuture: Future[String] = futureMonad.flatMap(future)(f => Future(f.concat(f)))

  /**
   * if we want to combine different types, we have to code
   * the same implementation for different types:
   *
   * def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
   *   numbers.flatMap(n => chars.map(c => (n, c)))
   * def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
   *   number.flatMap(n => char.map(c => (n, c)))
   * def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
   *   number.flatMap(n => char.map(c => (n, c)))
   */
  // Monads help generalize the problem above
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    assert(combinations1 == combinations2)
    assert(combined1 == combined2)
    println(transformed)
    println(transformedList)
    transformedFuture.foreach(println)
    // use monad
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}