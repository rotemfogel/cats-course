package alien

import cats.Monoid

import scala.annotation.unused

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(s: String): A
    def imap[B](fb: B => A, fa: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(fb(value))
      override def decrypt(s: String): B = fa(self.decrypt(s))
    }
  }

  // generic APIs
  def encrypt[A](a: A)(implicit c: Crypto[A]): String = c.encrypt(a)
  def decrypt[A](a: String)(implicit c: Crypto[A]): A = c.decrypt(a)

  implicit val ceaserCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(s: String): String = s.map(c => (c - 2).toChar)
  }

  /*
   * How can we support decrypt for Int, Double, Option[String] and more?
   */
  implicit val doubleCrypto: Crypto[Double] = ceaserCypher.imap(_.toString, _.toDouble)

  // TODO: create a crypto for Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] =
    ceaserCypher.imap(_.getOrElse(""), Option(_))

  // TODO: generalize the pattern - Crypto[T] => Crypto[Option[T]] if you have Monoid[T] in scope
  implicit def optionCrypto[T](implicit c: Crypto[T], m: Monoid[T]): Crypto[Option[T]] =
    c.imap(_.getOrElse(m.empty), Option(_))

  import cats.{Invariant, Show}
  val showString: Show[String] = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  // extension method
  import cats.syntax.invariant._
  val showOptionString2 = showString.imap(Option(_))(_.getOrElse(""))

  trait An_Invariant[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B]
  }
  @unused
  trait A_Contravariant[F[_]] extends An_Invariant[F] {
    def contramap[A, B](fa: F[A])(b: B => A): F[B]
    override def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B] = contramap(fa)(b)
  }
  @unused
  trait A_Functor[F[_]] extends An_Invariant[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    override def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B] = map(fa)(f)
  }

  def main(args: Array[String]): Unit = {
    val secret = "The Gold is in the Box"
    val encrypted = encrypt[String](secret)
    val decrypted = decrypt[String](encrypted)
    require(secret == decrypted)
    println(encrypted)
    println(decrypted)
    println("-" * 30)
    val dEnc: String = encrypt[Double](math.Pi)
    val dDec: Double = decrypt[Double](dEnc)
    require(math.Pi == dDec)
    println(dEnc)
    println(dDec)
    println("-" * 30)
    val oEnc: String = encrypt[Option[String]](Some(secret))
    val oDec: Option[String] = decrypt[Option[String]](oEnc)
    require(Some(secret) == oDec)
    println(oEnc)
    println(oDec)
    val nEnc: String = encrypt[Option[String]](None)
    val nDec: Option[String] = decrypt[Option[String]](nEnc)
    require(nDec.get.isEmpty)
    println(nEnc)
    println(nDec)
    println("-" * 30)
    val odEnc: String = encrypt[Option[Double]](Some(math.Pi))
    val odDec: Option[Double] = decrypt[Option[Double]](odEnc)
    require(Some(math.Pi) == odDec)
    println(odEnc)
    println(odDec)
    println("-" * 30)
  }
}