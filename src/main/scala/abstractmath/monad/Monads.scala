package abstractmath.monad

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaUnusedSymbol
object Monads {
  // lists
  val numbersList: List[Int] = List(1, 2, 3)
  val charsList: List[Char] = List('a', 'b', 'c')

  // TODO: create all combinations of (number, character)
  val combinations1: List[(Int, Char)] = numbersList.flatMap((n: Int) => charsList.map((c: Char) => (n, c)))
  val combinations2: List[(Int, Char)] =
    for {
      n <- numbersList
      c <- charsList
    } yield (n, c)


  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')

  // TODO: create a combinations of (number, character)
  val combined1: Option[(Int, Char)] = numberOption.flatMap((n: Int) => charOption.map((c: Char) => (n, c)))
  val combined2: Option[(Int, Char)] =
    for {
      n <- numberOption
      c <- charOption
    } yield (n, c)

  // futures
  val es: ExecutorService = Executors.newFixedThreadPool(6)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
  val numberFuture: Future[Int] = Future(2)
  val charFuture: Future[Char] = Future('d')

  // TODO: create a combinations of (number, character)
  val combinedFuture1: Future[(Int, Char)] = numberFuture.flatMap((n: Int) => charFuture.map((c: Char) => (n, c)))
  val combinedFuture2: Future[(Int, Char)] =
    for {
      n <- numberFuture
      c <- charFuture
    } yield (n, c)

  /**
   * We have 3 different data types (List/Option/Future) which can be combined
   * with the same patterns:
   * 1. wrap a value to a Monadic value
   * 2. use flatMap to transform Monadic values in sequence
   *
   * The type class the formalizes this behavior is a Monad
   *
   * A Monad also extends Functor, since it has a map method
   */
  //noinspection ScalaUnusedSymbol
  trait A_Monad[M[_]] {
    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO: implement the map method in A_Monad
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)((a: A) => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._

  val optionMonad: Monad[Option] = Monad[Option]
  val option: Option[Int] = optionMonad.pure(4) // Some(4)
  val transformed: Option[Int] = optionMonad.flatMap(option)((x: Int) => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._ // Monad[List]

  val listMonad: Monad[List] = Monad[List]
  val list: List[Int] = listMonad.pure(1) // List(1)
  val transformedList: List[Int] = listMonad.flatMap(list)((x: Int) => List(x, x + 1)) // List(1,2)

  // TODO: add Monad[Future]

  import cats.instances.future._

  val futureMonad: Monad[Future] = Monad[Future]
  val future: Future[String] = futureMonad.pure("HA") // Future[String]
  val transformedFuture: Future[String] = futureMonad.flatMap(future)((f: String) => Future(f.concat(f)))

  /**
   * if we want to combine different types, we have to code
   * the same implementation for different types:
   *
   * def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
   * numbers.flatMap(n => chars.map(c => (n, c)))
   * def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
   * number.flatMap(n => char.map(c => (n, c)))
   * def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
   * number.flatMap(n => char.map(c => (n, c)))
   */
  // Monads help generalize the problem above
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)((a: A) => monad.map(mb)((b: B) => (a, b)))

  // Monads extension methods - pure, flatMap

  import cats.syntax.applicative._

  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] - wrap 1 with Option -> Some(1)
  val oneList: List[Int] = 1.pure[List] // implicit Monad[List]
  val oneOptionTransformed: Option[Int] = oneOption.flatMap((x: Int) => (x + 1).pure[Option])

  // Monad extends Functor
  val oneOptionMapped: Option[Int] = Monad[Option].map(oneOption)((_: Int) + 1)
  // Since Monad has access to map (Functors) and flatMap
  // we can also support for comprehensions
  val combinedOptions: Option[Int] = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO: a shorter version of getPairs with for comprehensions

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPairsFor[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  //noinspection DuplicatedCode
  def main(args: Array[String]): Unit = {
    assert(combinations1 == combinations2)
    assert(combined1 == combined2)
    println(transformed)
    println(transformedList)
    transformedFuture.foreach(println)
    println("-" * 30)
    // use monad
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
    println("-" * 30)
    println(oneList)
    println(oneOptionTransformed)
    println(oneOptionMapped)
    println(combinedOptions)
    println("-" * 30)
    // getPairsFor
    println(getPairsFor(numbersList, charsList))
    println(getPairsFor(numberOption, charOption))
    getPairsFor(numberFuture, charFuture).foreach(println)

    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}