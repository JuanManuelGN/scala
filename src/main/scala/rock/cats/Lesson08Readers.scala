package rock.cats

object Lesson08Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */
  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )
  // DB layer
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String =
      "dispatched" // Select * from the db table and return the status of the orderID
    def getLastOrderId(username: String): Long = 574673
  }
  // Http layer
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config = Configuration("juan", "navarro2!", "localhost", 123, 8, "juan@navarrosoft.es")
  // using cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O], Reader es una wrapper sobre una función entre I (input) y O (output)
  val juanOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(dbCon => dbCon.getOrderStatus(55))
  val juanOrderStatus: String = juanOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(dbcon => dbcon.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical:
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

//    usersLastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  /*
    Pattern
    1. create the initial data structure
    2. create a reader which specifies how that data structure will be manipulated later
    3. you can the map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with initial structure
   */

  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From: $emailReplyTo to $address > $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    // my way
//    val emailReader = for {
//      // fetch the status of their last order
//      orderId     <- dbReader.map(_.getLastOrderId(username))
//      orderStatus <- dbReader.map(_.getOrderStatus(orderId))
//      // email them with the Email service: "Your last order has the status: <status>"
//      emailService = EmailService(config.emailReplyTo)
//    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")
//
//    emailReader.run(config)

    // course way
    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      orderId      <- dbReader.map(_.getLastOrderId(username))
      orderStatus  <- dbReader.map(_.getOrderStatus(orderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")

    emailReader.run(config)
  }
  
  // TODO: what programming pattern do Readers remind you of?
  // Dependency injection

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("juan"))
    println(emailUser("juan", "destiny@lk.com"))
  }

}
