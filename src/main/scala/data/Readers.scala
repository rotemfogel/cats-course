package data

object Readers {
  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */
  case class Configuration(dbUsername: String,
                           dbPassword: String,
                           host: String,
                           port: Int,
                           nThreads: Int,
                           emailReplyTo: String)

  //noinspection ScalaUnusedSymbol
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the db table and return the status of the orderID
    def getLastOrderId(userId: Long): Long = 542643 // select max(orderId) from table where user_id = userId
  }

  //noinspection ScalaUnusedSymbol
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config: Configuration = Configuration("daniel", "rockthejvm1!", "localhost", 8080, 8, "daniel@rockthejvm.com")

  // cats Reader
  import cats.Id
  import cats.data.Reader

  val userId: Long = 1L
  // Reader[I, O] -> a function from I: Input to O: Output
  val dbReader: Reader[Configuration, DbConnection] = Reader((conf: Configuration) => DbConnection(conf.dbUsername, conf.dbPassword))
  // inject the config to the reader, run it and get the db connection
  val dbConnection: Id[DbConnection] = dbReader.run(config)
  // We can use map to execute a function on the reader object, when it obtains the DbConnection
  val maxOrderId: Id[Long] = dbReader.map((con: DbConnection) => con.getLastOrderId(1L)).run(config)

  /*
    Reader[I,O] Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // get last order status
  val maxOrderIdStatus: Reader[Configuration, String] = for {
    orderId <- dbReader.map((_: DbConnection).getLastOrderId(userId))
    status <- dbReader.map((_: DbConnection).getOrderStatus(orderId))
  } yield status

  case class EmailService(emailReplyTo: String) {
    def sendEmail(email: String, content: String): String =
      s"""
         |FROM: $emailReplyTo
         |  TO: $email
         |      $content
         |""".stripMargin.trim
  }

  // TODO
  def emailUser(userId: Long, userEmail: String): Id[String] = {
    // fetch status of the last order
    // email them to the email service: "Order Id $orderId has the following status: $orderStatus"
    val emailReader: Reader[Configuration, EmailService] = Reader((conf: Configuration) => EmailService(conf.emailReplyTo))
    val email: Reader[Configuration, String] = for {
      orderId <- dbReader.map((_: DbConnection).getLastOrderId(userId))
      orderStatus <- dbReader.map((_: DbConnection).getOrderStatus(orderId))
      email <- emailReader.map((_: EmailService).sendEmail(userEmail, s"Order Id $orderId has the following status: $orderStatus"))
    } yield email
    email.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(dbConnection)
    println(maxOrderId)
    println(maxOrderIdStatus.run(config))
    println("-" * 30)
    println(emailUser(userId, "info@acme.com"))
  }
}