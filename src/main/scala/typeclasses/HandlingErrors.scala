package typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

//noinspection DuplicatedCode
// there are 3 types of error handling
// 1. try/catch blocks
// 2. Using scala.util.Try
// 3. Pure FP with Cats

//noinspection ScalaUnusedSymbol
object HandlingErrors {

  /**
   *
   * @tparam F - higher kinded type wrapper
   * @tparam E - Error
   */
  trait An_ApplicativeError[F[_], E] extends Applicative[F] {
    // pure from Applicative
    def raiseError[A](e: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa)((v: E) => pure(f(v)))
  }
  trait A_MonadError[F[_], E] extends An_ApplicativeError[F, E] with Monad[F] {
    def ensure[A](fa: F[A])(e: E)(p: A => Boolean): F[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String] // the String must be equal to Either[String]
  val success: ErrorOr[Int] = monadErrorEither.pure(42) // Right(42)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something went wrong") // Left("Something went wrong")
  // equivalent to recover
  val onError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Something went wrong" => -1
    case _ => -999
  } // Right(-1)
  // equivalent to recoverWith
  val handledError: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "blah" => monadErrorEither.pure(-1)
    case _ => Left("-999") // because ErrorOr is an Either[String, Int]
  } // Right(-999)
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")((_: Int) < 100) // Right(42)

   // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try]
  val exception = new RuntimeException("This is really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  import cats.instances.future._ // implicit MonadError[Future]
  val es: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
  val futureMonad: Future[Nothing] = MonadError[Future, Throwable].raiseError(exception) // Future[Failure(exception)]

  // for applicative => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applicativeErrorValidated: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]
  // have access to: pure, raiseError, handleError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._ // import raiseError, handleError, handleErrorWith
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // implicit ApplicativeError[ErrorsOr, ...]
  val extendedErrors: ErrorsOr[Int] = List("bad").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedErrors.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // import ensure
  val testSuccess = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {
    println(success)
    println(failure)
    println(onError)
    println(handledError)
    println(filteredSuccess)
    futureMonad.foreach(println)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}