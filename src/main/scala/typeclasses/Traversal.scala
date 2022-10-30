package typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaUnusedSymbol
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
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = {
    list.foldLeft(List.empty[A].pure[F])((l, a) => (l, a).mapN(_ :+ _))
    // or
    // listTraverseApplicative(list)(identity)
  }

  // TODO: What is the result?
  import cats.instances.vector._
  val result1: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4)))
  // Vector(List(1,3),List(1,4),List(2,3), List(2,4))
  val result2: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

  /**
   * extract the inner wrapper outside and apply
   * all the possible tuples between the elements
   */

  def main(args: Array[String]): Unit = {
    allBandwidth.foreach(println)
    allBandwidthTraverse.foreach(println)
    allBandwidthSequence.foreach(println)
    println("-" * 30)
    println(result1)
    println(result2)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }
}