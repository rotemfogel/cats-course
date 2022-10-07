package abstractmath.monad

//noinspection ScalaUnusedSymbol
object UsingMonads {

  import cats.Monad

  // Either - Left is used for failure and Right is used for success.
  val manualEither: Either[String, Int] = Right(42)
  type ErrorOr[T] = Either[String, T]
  val errorOr = Monad[ErrorOr]
  val either = errorOr.pure(1) // Right(1)
  val changedEither = errorOr.flatMap(either)(_ => Left("error"))

  // online store
  case class OrderStatus(id: Long, status: String)

  def getOrderStatus(id: Long): ErrorOr[OrderStatus] = Right(OrderStatus(id, "ready to ship"))

  def trackLocation(orderStatus: OrderStatus): ErrorOr[String] =
    if (orderStatus.id > 1000) Left("Not Available Yet")
    else Right("Amsterdam, Netherlands")

  // let's combine the two methods
  val orderId: Long = 284L
  val orderLocation = errorOr.flatMap(getOrderStatus(orderId))(trackLocation)
  val orderLocationBetter = getOrderStatus(orderId).flatMap(trackLocation)
  // using for comprehension
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: service layer API for a web application
  case class Connection(host: String, port: String)
  val config: Map[String, String] = Map("host" -> "localhost", "port" -> "8080")

  /*
   * requirements:
   *   getConnection - if the host and port are found in the configuration map, return an M
   *                   containing those values, otherwise the method will fail according to the M type
   *                   (for Try it will return Failure, for Option None, for Future a failed Future,
   *                    for Either, it will return a Left
   *   issueRequest - if the payload less than 20 characters, return an M[message],
   *                  otherwise, fail the method the same way in getConnection
   *
   */
  trait HttpService[M[_]] {
    def getConnection(config: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  implicit object OptionHttpServer extends HttpService[Option] {
    override def getConnection(config: Map[String, String]): Option[Connection] =
      for {
        h <- config.get("host")
        p <- config.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"request [$payload] accepted")
      else None
  }

  val payload = "Hello HTTP"
  val errorPayload = s"$payload Error"

  val responseOption = {
    OptionHttpServer.getConnection(config)
      .flatMap(conn => OptionHttpServer.issueRequest(conn, payload))
  }

  val responseOptionFor = for {
    conn <- OptionHttpServer.getConnection(config)
    response <- OptionHttpServer.issueRequest(conn, payload)
  } yield response

  implicit object ErrorHttpServer extends HttpService[ErrorOr] {
    override def getConnection(config: Map[String, String]): ErrorOr[Connection] = {
      if (!config.contains("host") || !config.contains("port"))
        Left("Invalid Configuration")
      else
        Right(Connection(config("host"), config("port")))
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"request [$payload] accepted")
      else Left("")
  }

  val errorResponseOption = for {
    conn <- ErrorHttpServer.getConnection(config)
    response <- ErrorHttpServer.issueRequest(conn, errorPayload)
  } yield response

  // generalize with Monads
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  def main(args: Array[String]): Unit = {
    println(either)
    println(changedEither)
    println("-" * 30)
    println(orderLocation)
    println(orderLocationBetter)
    println(orderLocationFor)
    println("-" * 30)
    println(responseOption)
    println(responseOptionFor)
    println("-" * 30)
    println(errorResponseOption)
    println("-" * 30)
    println(getResponse(OptionHttpServer, payload))
    println(getResponse(ErrorHttpServer, errorPayload))
  }
}