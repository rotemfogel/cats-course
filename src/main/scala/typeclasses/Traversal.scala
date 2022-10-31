package typeclasses

import cats.{Applicative, Foldable, Monad}

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.annotation.unused
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaUnusedSymbol,DuplicatedCode
object Traversal {

  val es: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  val servers: List[String] = List(
    "server-ci.rockthejvm.com",
    "server-staging.rockthejvm.com",
    "prod.rockthejvm.com"
  )

  val getBandwidth: String => Future[Int] = (hostname: String) => Future(hostname.length * 17)

  /*
   * we have:
   *   - a List[String]
   *   - a function String => Future[Int] (getBandwidth)
   * we want
   *   - Future[List[Int]]
   */
  // TODO: implement with foldLeft
  def allBandwidth: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) {
    (a: Future[List[Int]], b: String) =>
      for {
        acc <- a
        band <- getBandwidth(b)
      } yield acc :+ band
  }

  // traverse method will automatically combine all the results of getBandwidth
  def allBandwidthTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  // unwrap a list of futures to a future of list
  def allBandwidthSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO: implement

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def listTraverseMonad[F[_] : Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (l: F[List[B]], a: A) =>
        for {
          list <- l
          e <- f(a)
        } yield list :+ e
    }

  import cats.syntax.apply._
  def listTraverseApplicative[F[_] : Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])((l: F[List[B]], a: A) => (l, f(a)).mapN((_: List[B]) :+ (_: B)))

  // TODO: implement
  /**
   * extract the inner wrapper outside and apply
   * all the possible tuples between the elements
   */
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = {
    list.foldLeft(List.empty[A].pure[F])((l: F[List[A]], a: F[A]) => (l, a).mapN((_: List[A]) :+ (_: A)))
    // or
    // listTraverseApplicative(list)(identity)
  }

  // TODO: What is the result?
  import cats.instances.vector._
  val result1: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4)))
  // Vector(List(1,3),List(1,4),List(2,3), List(2,4))
  val result2: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // Vector(List(1,3,5), List(1,3,6), List(1,4,5), List(1,4,6), List(2,3,5), List(2,3,6), List(2,4,5), List(2,4,6))

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverseApplicative[Option, Int, Int](list)((n: Int) => Some(n).filter(predicate))

  // TODO: What is the output of this?
  val fResult1: Option[List[Int]] = filterAsOption(List(2, 4, 6))((_: Int) % 2 == 0) // Some(List(2,4,6))
  val fResult2: Option[List[Int]] = filterAsOption(List(1, 2, 3))((_: Int) % 2 == 0) // None - since combining None and Some yields None

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseApplicative[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"$n failed predicate"))
    }

  // TODO: What is the result?
  val fResult3: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))((_: Int) % 2 == 0) // Valid(List(2,4,6))
  val fResult4: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))((_: Int) % 2 == 0) // Invalid(List("1 failed..","3 failed.."))

  @unused
  trait A_Traverse[L[_]] extends Foldable[L]{
    def traverse[F[_]: Applicative, A, B](container: L[A])(f: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)
    // TODO: implement using traverse and/or sequence
    import cats.Id
    def map[A, B](la: L[A])(f: A => B): L[B] = traverse[Id, A, B](la)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth) // implicit Traverse[List]

  // extension methods
  import cats.syntax.traverse._
  val bandwidthCats = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    allBandwidth.foreach(println)
    allBandwidthTraverse.foreach(println)
    allBandwidthSequence.foreach(println)
    println("-" * 30)
    println(result1)
    println(result2)
    println("-" * 30)
    println(fResult1)
    println(fResult2)
    println("-" * 30)
    println(fResult3)
    println(fResult4)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}