package data

//noinspection ScalaUnusedSymbol
object Evaluation {
  /*
    Cats makes the distinction between
    - evaluating an expression eagerly (default: val x = 2+3)
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoization)
   */

  import cats.Eval

  val eagerEval: Eval[Int] = Eval.now {
    println("evaluating eagerly")
    21
  }

  // recompute every time you request the value
  val alwaysEval: Eval[Int] = Eval.always {
    println("evaluating always")
    34
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("computing lazily")
    42
  }

  val composedEval: Eval[Int] = // instant.flatMap(v1 => delayedEval.map(v2 => v1 + v2))
    for {
      v1 <- eagerEval
      v2 <- delayedEval
    } yield v1 + v2

  // TODO: what will be printed to console?
  val ex1: Eval[Int] =
    for {
      a <- delayedEval
      b <- alwaysEval
      c <- eagerEval
      d <- alwaysEval
    } yield a + b + c + d
  // eager only (on first evaluation)
  // when calling ex1.value:
  // lazily, always, always (eagerly was already evaluated), sum()

  // eval can remember a computed value
  val noRecompute: Eval[Int] = alwaysEval.memoize

  val tutorial: Eval[String] =
    Eval
      .always {
        println("Step #1")
        "put the guitar on your lap."
      }
      .map { step1: String =>
        println("Step #2")
        s"$step1 put the left hand on the neck."
      }
      .memoize // remember the value up to this point
      .map { step2: String =>
        println("Step #3")
        s"$step2 strum the strings with your right hand."
      } // repeat this value when evaluated

  // TODO: implement defer such as defer(Eval.now(...)) does not run side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap((_: Unit) => eval)

  // TODO: rewrite method with Eval
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  // stack recursive
  def reverseListEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseListEval(list.tail).map((_: List[T]) :+ list.head)


  // when deferring evaluation, Eval will chain the evaluation as such:
  // Eval.later(expr1 => _.flatMap(expr2 => _.flatMap(...
  // so evaluation will be done in a tail recursive way
  def reverseListEvalDeferred[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseListEvalDeferred(list.tail).map((_: List[T]) :+ list.head))

  //noinspection DuplicatedCode
  def main(args: Array[String]): Unit = {
    //    println(eagerEval.value)
    //    println(eagerEval.value) // println runs only once
    //    println("-" * 30)
    // println runs every time (expression is evaluated every time)
    println(alwaysEval.value)
    println(alwaysEval.value)
    println("-" * 30)
    // will evaluate lazily, but will keep the evaluated value in memory
    println(delayedEval.value)
    println(delayedEval.value)
    println("-" * 30)
    //    println(composedEval.value)
    //    println(composedEval.value)
    //    println(ex1.value)
    //    println("-" * 30)
    //    // second time it will evaluate the expression again
    //    // it will print always, always, sum()
    //    println(ex1.value)
    //    println("-" * 30)
    println(noRecompute.value)
    println(noRecompute.value)
    println("-" * 30)
    println(tutorial.value)
    println(tutorial.value)
    println("-" * 30)
    // should delay print of defer now
    defer(Eval.now {
      println("defer now")
      42
    })
    // will evaluate
    defer(Eval.now {
      println("defer now")
      42
    }).value
    println("-" * 30)
    val list: List[Int] = Range.inclusive(1, 100).toList
    println(reverseList(list))
    println("-" * 30)
    println(reverseListEval(list).value)
    val bigList: List[Int] = Range.inclusive(1, 10000).toList
    // will cause stack to overflow
    //    println(reverseList(bigList))
    println(reverseListEvalDeferred(bigList).value)
  }
}