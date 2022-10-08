package abstractmath.monad

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.Future

//noinspection ScalaUnusedSymbol
object MonadTransformers {

  //noinspection NotImplementedCode
  def sumAllOptions(list: List[Option[Int]]): Int = ???
  // in order to execute the sum on all options, we will need
  // to unwrap every element in the list at least once
  // e.g. list.filter(_.nonEmpty).map(_.get).sum

  /**
   * A monad transformer will provide you with a
   * map and flatMap methods, so you don't have to
   * unwrap and wrap the inner monad
   */

  import cats.data.OptionT
  import cats.instances.list._ // fetch OptionT[List]

  val listOfNumbersOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  // if we want to combine the two lists, we will need to unwrap very element
  // in each list and wrap it again
  val listOfTuples: OptionT[List, (Int, Char)] =
  for {
    number <- listOfNumbersOptions
    char <- listOfCharOptions
  } yield (number, char)

  // Either transformer
  import cats.data.EitherT

  import scala.concurrent.ExecutionContext

  val es: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("Blat"), Right(42)))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  /*
     TODO exercise
      We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
      We measure bandwidth in units.
      We want to allocate TWO of our servers to cope with the traffic spike.
      We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val server1: String = "server1.rockthejvm.com"
  val server2: String = "server2.rockthejvm.com"
  val server3: String = "server3.rockthejvm.com"
  val bandwidths: Map[String, Int] = Map(
    server1 -> 50,
    server2 -> 300,
    server3 -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]
  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None => EitherT.left(Future(s"server [$server] unavailable"))
      case Some(b) => EitherT.right(Future(b))
  }
  // TODO 1
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      band1 <- getBandwidth(s1)
      band2 <- getBandwidth(s2)
    } yield band1 + band2 > 250

  // TODO 2
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"server [$s1 and $s2] cannot cope with spike: $reason")
      case Right(false) => Left(s"server [$s1 and $s2] cannot cope with spike: Not Enough Total Bandwidth")
      case Right(true) => Right(s"server [$s1 and $s2] can cope with spike")
    }
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    println("-" * 30)
    generateTrafficSpikeReport(server1, server2).value.foreach(println)
    generateTrafficSpikeReport(server2, server3).value.foreach(println)
    generateTrafficSpikeReport(server1, server3).value.foreach(println)
    generateTrafficSpikeReport("server4.rockthejvm.com", server3).value.foreach(println)
    es.awaitTermination(1, TimeUnit.SECONDS)
    es.shutdown()
  }

}