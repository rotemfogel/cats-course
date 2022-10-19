package data

import scala.annotation.tailrec
import scala.util.{Failure, Success}

//noinspection ScalaUnusedSymbol
object DataValidations {

  import cats.data.Validated

  // validated acts like and Either[E, A] (E = Error, A = A Value)
  val validValue: Validated[String, Int] = Validated.valid(24) // "right" value
  val invalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value

  val test: Validated[String, Int] = Validated.cond(42 > 30, 1, "error")

  def isPrime(n: Int): Boolean = {
    @tailrec def isPrime(d: Int): Boolean = {
      if (d <= 1) true
      else n % d != 0 && isPrime(d - 1)
    }
    if (n == -1 || n == 0 || n == 1) false
    else isPrime(math.abs(n / 2))
  }

  // TODO: use Either for the following scenario
  /**
   * test if the n is a prime, non-negative, n <= 100, and even
   *
   * @param n - integer
   * @return Either[List[String], Int]
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val p: List[String] = if (isPrime(n)) List.empty[String] else List("Number must be prime")
    val gt: List[String] = if (n > 0) List.empty[String] else List("Number must be positive")
    val lt: List[String] = if (n <= 100) List.empty[String] else List("Number must be less than 100")
    val e: List[String] = if (n % 2 == 0) List.empty[String] else List("Number must be even")

    val result: List[String] = gt ++ lt ++ p ++ e
    if (result.isEmpty) Right(n) else Left(result)
  }

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(isPrime(n), n, List("Number must be prime"))
      .combine(Validated.cond(n > 0, n, List("Number must be positive")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than 100")))
      .combine(Validated.cond(n % 2 == 0, n, List("Number must be even")))

  // chain - we use andThen
  // if we use flatMap, and the starting value is invalid
  // flatMap would short circuit the chain and no errors
  // will accumulate
  val chain: Validated[String, Int] = validValue.andThen((_: Int) => invalidValue)
  // test valid value
  val ensured: Validated[Serializable, Int] = validValue.ensure(List("ensure did not work"))((_: Int) % 2 == 0)
  // transform
  // transform the left value
  val lTransformed: Validated[String, Int] = validValue.map((_: Int) + 1)
  // transform the right value
  val rTransformed: Validated[Int, Int] = invalidValue.leftMap((_: String).length)
    // transform both
  val bTransformed: Validated[Int, Int] = validValue.bimap((_: String).length, (_: Int) + 1)

  // interoperate with standard library
  val fromRight: Validated[Nothing, Int] = Validated.fromEither(Right(42))
  val fromLeft: Validated[String, Nothing] = Validated.fromEither(Left("Must have value"))
  val fromSome: Validated[String, Int] = Validated.fromOption(Some(42), "Must have value")
  val fromNone: Validated[String, Nothing] = Validated.fromOption(None, "Must have value")
  val fromSuccess: Validated[Throwable, Int] = Validated.fromTry(Success(42))
  val fromFailure: Validated[Throwable, Nothing] = Validated.fromTry(Failure(new RuntimeException("Must have value")))

  // TODO: form validation exercise
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]
    val name: String = "Name"
    val email: String = "Email"
    val password: String = "Password"

    private def getValue(form: Map[String, String], field: String): FormValidation[String] =
      Validated.fromOption(form.get(field), List(s"$field must be specified"))

    private def nonBlank(value: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List("field must not be empty"))

    private def properEmail(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("invalid email"))

    private def properPassword(password: String): FormValidation[String] =
      Validated.cond(password.length>=10, password, List("invalid email"))

    /**
     * validate form
     * form must contain name, email, password
     * rules:
     * - name must not be blank
     * - email must be a valid email (must contain @)
     * - password must have at least 10 characters
     * @param form Form
     * @return FormValidation
     */
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, name).andThen(nonBlank)
        .combine(getValue(form, email).andThen(properEmail))
        .combine(getValue(form, password).andThen(properPassword))
        .map((_: String) => "success")
    }
  }

  // validated extension methods
  import cats.syntax.validated._
  // valid accepts the value and error type
  // invalid accepts the error and the value type
  val validMeaningOfLife: Validated[List[String], Int] = 24.valid[List[String]]
  val error: Validated[String, Int] = "Something went wrong".invalid[Int]

  //noinspection DuplicatedCode
  def main(args: Array[String]): Unit = {
    println(validValue)
    println(invalidValue)
    println(test)
    println("-" * 30)
    println(testNumber(1))
    println(testNumber(2))
    println(testNumber(3))
    println("-" * 30)
    println(validateNumber(1))
    println(validateNumber(2))
    println(validateNumber(3))
    println("-" * 30)
    println(chain)
    println(ensured)
    println(lTransformed)
    println(rTransformed)
    println(bTransformed)
    println("-" * 30)
    println(fromRight)
    println(fromLeft)
    println(fromSome)
    println(fromNone)
    println(fromSuccess)
    println(fromFailure)
    println("-" * 30)
    println(fromRight.toEither)
    println(fromLeft.toEither)
    println(fromSome.toOption)
    println(fromNone.toOption)
    println("-" * 30)
    import FormValidation._
    println(validateForm(Map(name -> "")))
    println(validateForm(Map(name -> "aa", email -> "aa")))
    println(validateForm(Map(name -> "aa", email -> "a@b")))
    println(validateForm(Map(name -> "aa", email -> "a@b", password -> "aa")))
    println(validateForm(Map(name -> "aa", email -> "a@b", password -> "a" * 10)))
  }
}