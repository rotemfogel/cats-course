package typeclasses

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaUnusedSymbol
object Semigroupals {
  // a Semigroupal data type works on higher kinded types
  // and tuples two elements together

  //noinspection ScalaUnusedSymbol
  trait A_Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemi: Semigroupal[Option] = Semigroupal[Option]
  val tupledOption: Option[(Int, String)] = optionSemi.product(Some(123), Some("a string")) // (123, "a string")
  val noneTuples: Option[(Int, Nothing)] = optionSemi.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  val es: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
  val futureSemi: Future[(String, Int)] = Semigroupal[Future].product(Future("meaning of life"), Future(42))

  import cats.instances.list._
  val tupledList: List[(Int, String)] = Semigroupal[List].product(List(1,2), List("a", "b"))

  // TODO: implement
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
    // in other words:
    // monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
  }

  // Monads extend Semigroupal
  // Validated is a good way of combining instances of type validated
  // without needing to follow the Monad laws
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr] // requires implicit Semigroup[List[_]]
  // Semigroupal[ErrorsOr] will implement product of two Validated instances
  // combining List[String] and T (value) according to their Semigroup

  // example: combine invalid Validated
  val invalidCombinations = validatedSemigroupal.product(
    Validated.invalid(List("something wrong", "something else wrong")),
    Validated.invalid(List("this can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigruopal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigruopal.product( // implemented using Monadic law (map/flatMap)
    Left(List("something wrong", "something else wrong")),
    Left(List("this can't be right"))
  )
  // as a result, the last error will not be propagated
  // because the flatMap short circuits the Either Monadic type

  // TODO: define a Semigroupal[List] that does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(tupledOption)
    println(noneTuples)
    futureSemi.foreach(println)
    val result: List[(Int, String)] = productWithMonads(List(1,2), List("a", "b"))
    assert(tupledList == result)
    println(result)
    println("-" * 30)
    println(invalidCombinations)
    println(eitherCombination)
    require(eitherCombination.left.getOrElse(List()).length == 2)
    println("-" * 30)
    println(zipListSemigroupal.product(List(1,2), List("a", "b")))
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}