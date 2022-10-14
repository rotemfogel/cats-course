package data

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Writers {
  /*
    A Writer is a function which enables managing log values.
    When two functions are composed together (e.g. using flatMap), the logs of both functions will be combined
    using an implicit Semigroup.
   */

  import cats.data.Writer
  import cats.implicits.catsSyntaxSemigroup

  val input: Int = 1
  // define the writer
  val aWriter: Writer[String, Int] = Writer[String, Int]("+1", input)
  // manipulate them using FP
  val addOneWriter: Writer[String, Int] = aWriter.map((_: Int) + 1) // logs will stay the same, value changes
  // extract the value or the logs
  val value: Int = aWriter.value
  val log: String = aWriter.written
  // extract both at the same time
  val addOneResult: (String, Int) = addOneWriter.run
  // for comprehension
  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log a1", "Log a2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log b1"), 20)
  val writerFor: Writer[Vector[String], Int] = for {
    valueA <- writerA
    valueB <- writerB
  } yield valueA + valueB

  // combine
  val combined: Writer[String, Int] = addOneWriter |+| addOneWriter
  val combinedResult: (String, Int) = combined.run

  // reset the logs
  val initialWriter: Writer[Vector[String], Int] = writerA.reset
  require(initialWriter.value == 10)
  /**
   * change writers
   */
  // logs changes, value stays
  val changedWriter1: Writer[String, Int] = aWriter.mapWritten((_: String) => "add one")
  // takes 2 functions, both logs and values change
  val changedWriter2: Writer[String, Int] = aWriter.bimap((_: String) => "add one", (_: Int) + 1)
  // takes single function, both logs and values change, can use old values
  val changedWriter3: Writer[String, Int] = aWriter.mapBoth((log: String, value: Int) => (log.concat(" and another one"), value + 1))

  // TODO: rewrite a function that prints something with Writer[T]
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }
  def countAndSayWriter(n: Int): Writer[Vector[String], Int] = {
    @tailrec def rec(writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (writer.value == 0) writer.mapWritten((_: Vector[String]) :+ "starting")
      else rec(writer.mapBoth((log: Vector[String], value: Int) => (log :+ value.toString, value - 1)))
    }
    rec(Writer(Vector(), n))
  }

  /*
    benefits of using Writers:
    1. work with pure FP (no side effects)
    2. Writers can keep logs separate on multiple threads (see line #112)
   */

  // TODO: rewrite the function with Writer[T]
  def naiveSum(n: Int): Int = {
    if (n == 0) 0
    else {
      println(s"now at $n")
      val lowerSum: Int = naiveSum(n-1)
      println(s"computed sum(${n - 1})=$lowerSum")
      lowerSum + n
    }
  }

  def naiveSumWriter(n: Int): Writer[List[String], Int] = {
    if (n == 0) Writer(List(), 0)
    else for {
      _ <- Writer(List(s"now at $n"), n)
      lowerSum <- naiveSumWriter(n - 1)
      _ <- Writer(List(s"computed sum(${n - 1})=$lowerSum"), lowerSum+n)
    } yield lowerSum + n
  }

  def main(args: Array[String]): Unit = {
    println(s"value: $value, log: $log")
    println(s"result of $input${addOneResult._1}=${addOneResult._2}")
    println("-" * 30)
    println(s"combined result ($input${addOneResult._1})+($input${addOneResult._1})=${combinedResult._2}")
    println("-" * 30)
    println(s"combined: ${writerFor.written}, ${writerFor.value}")
    println(changedWriter1.run)
    println(changedWriter2.run)
    println(changedWriter3.run)
    println("-" * 30)
    val result: Writer[Vector[String], Int] = countAndSayWriter(5)
    result.written.reverse.foreach(println)
    println("-" * 30)
    val naiveSumResult: Writer[List[String], Int] = naiveSumWriter(5)
    naiveSumResult.written.foreach(println)
    println("-" * 30)

    val sc: ExecutorService = Executors.newFixedThreadPool(8)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(sc)
    // when stack recursive code runs in parallel, the logs are mixed up due to side effects
    Future(naiveSum(50)).foreach(println)
    Future(naiveSum(50)).foreach(println)
    Thread.sleep(1000)
    println("-" * 30)
    Future(naiveSumWriter(50).written.foreach(println))
    Future(naiveSumWriter(50).written.foreach(println))

    sc.awaitTermination(3, TimeUnit.SECONDS)
    sc.shutdown()
  }
}