package com.komsonandmarch.typeclasses


trait Monoid[T] {
  def empty: T
  def combine(x: T, y: T): T
}

given Monoid[List[Int]] with
  def empty: List[Int] = Nil
  def combine(x: List[Int], y: List[Int]): List[Int] = x ++ y

trait Numeric[T]:
  def zero: T
  def plus(x: T, y: T): T

given Numeric[Int] with
  def zero: Int = 0
  def plus(x: Int, y: Int): Int = x + y

extension [T](xs: List[T])(using M: Monoid[T])
  def combineAll: T = xs.foldLeft(M.empty)(M.combine)

extension [T](xs: List[T])(using N: Numeric[T])
  def sum: T = xs.foldLeft(N.zero)(N.plus)
given Monoid[Int] with
    def empty: Int = 0
    def combine (x:Int, y: Int): Int = x + y

given Monoid[String] with
    def combine (x:String, y: String): String = x.concat(y)
    def empty: String = ""



def sum[T](items: List[T])(using m: Monoid[T]): T =
    items.foldLeft(m.empty)(m.combine)

trait Monad[F[_]]:
  def pure[A](value: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  extension [A](value: F[A])
    def map[B](func: A => B): F[B] = flatMap(value)(a => pure(func(a)))

object Monad {
  def apply[M[_]: Monad]: Monad[M] = summon[Monad[M]]

  given Monad[Option] with {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }
}

def safeDivide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)

enum Mood:
    case Surprised, Angry, Neutral
val errmsg =
    "Please enter a word, a positive integer count, and\n" +
    "a mood (one of 'angry', 'surprised', or 'neutral')"

object Mood:
    import scala.util.CommandLineParser.FromString

    given moodFromString: FromString[Mood] with
      def fromString(s: String): Mood =
        s.trim.toLowerCase match
          case "angry" => Mood.Angry
          case "surprised" => Mood.Surprised
          case "neutral" => Mood.Neutral
          case _ => throw new IllegalArgumentException(errmsg)

def repeat(word: String, count: Int)(using mood: Mood) =
    val msg =
      if count > 0 then
        val words = List.fill(count)(word.trim)
        val punc =
          mood match
            case Mood.Angry => "!"
            case Mood.Surprised => "?"
            case Mood.Neutral => ""
        val sep = punc + " "
        words.mkString(sep) + punc
      else errmsg

    println(msg)


trait JsonSerializer[T]:
    def serialize(o: T): String

    extension (a: T)
        def toJson: String = serialize(a)

object JsonSerializer:

    given stringSerializer: JsonSerializer[String] with
        def serialize(s: String) = s"\"$s\""

    given intSerializer: JsonSerializer[Int] with
        def serialize(n: Int) = n.toString

    given longSerializer: JsonSerializer[Long] with
        def serialize(n: Long) = n.toString

    given booleanSerializer: JsonSerializer[Boolean] with
        def serialize(b: Boolean) = b.toString

    given listSerializer[T](using
        JsonSerializer[T]): JsonSerializer[List[T]] with
        def serialize(ts: List[T]) =
            s"[${ts.map(t => t.toJson).mkString(", ")}]"

         

object ToJsonMethods:
  extension [T](a: T)(using jser: JsonSerializer[T])
    def toJson: String = jser.serialize(a)

case class Address(
  street: String,
  city: String,
  state: String,
  zip: Int
)

case class Phone(
  countryCode: Int,
  phoneNumber: Long
)

case class Contact(
  name: String,
  addresses: List[Address],
  phones: List[Phone]
)

case class AddressBook(contacts: List[Contact])

object Contact:
  given contactSerializer: JsonSerializer[Contact] with
    def serialize(c: Contact) =
      import ToJsonMethods.{toJson as asJson}
      s"""|{
          |  "name": ${c.name.asJson},
          |  "addresses": ${c.addresses.asJson},
          |  "phones": ${c.phones.asJson}
          |}""".stripMargin

object AddressBook:
  given addressBookSerializer: JsonSerializer[AddressBook] with
    def serialize(a: AddressBook) =
      import ToJsonMethods.{toJson as asJson}
      s"""|{
          |  "contacts": ${a.contacts.asJson}
          |}""".stripMargin

object Address:
  given addressSerializer: JsonSerializer[Address] with
    def serialize(a: Address) =
      import ToJsonMethods.{toJson as asJson}
      s"""|{
          |  "street": ${a.street.asJson},
          |  "city": ${a.city.asJson},
          |  "state": ${a.state.asJson},
          |  "zip": ${a.zip.asJson}
          |}""".stripMargin

object Phone:
  given phoneSerializer: JsonSerializer[Phone] with
    def serialize(p: Phone) =
      import ToJsonMethods.{toJson as asJson}
      s"""|{
          |  "countryCode": ${p.countryCode.asJson},
          |  "phoneNumber": ${p.phoneNumber.asJson}
          |}""".stripMargin


def Chap23 = 
    // monoid
    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)
    val result_int = combineAll(List(list1, list2)) // List(1, 2, 3, 4, 5, 6)    
    val numbers = List(1, 2, 3, 4, 5)
    val result_str = combineAll(numbers)
    println(result_int)
    println(result_str)
    // monad
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(6, 7, 8, 9, 10)
    val lst3 = List(11, 12, 13, 14, 15)

    val result = for {
      r1 <- lst1
      r2 <- lst2
      r3 <- lst3
    } yield r1 + r2 + r3

    val sum = combineAll(result)

    val fm_result: Option[Int] = Monad[Option].flatMap(safeDivide(6, 3))(x => safeDivide(12, x))
    val sum_result: Option[Int] = fm_result.map(_ + 1)
    println(sum_result)
    
    //val mood: Mood = Mood.moodFromString.fromString("angry")
    //val mood: Mood = Mood.moodFromString.fromString("angry")
    given mood: Mood = Mood.Angry
    repeat("hello",3)
    {
    given mood2: Mood = Mood.Neutral
    repeat("hello",3)
    }

    import ToJsonMethods.*
    val j1 = "tennis".toJson // "tennis"
    val j2 = 10.toJson // 10
    val j3 = true.toJson // true
    val j4 = List(1, 2, 3).toJson // [1, 2, 3]
    println(s"$j1, $j2, $j3 $j4")
    val addressBook =
    AddressBook(
      List(
        Contact(
          "Bob Smith",
          List(
            Address(
              "12345 Main Street",
              "San Francisco",
              "CA",
              94105
            ),
            Address(
              "500 State Street",
              "Los Angeles",
              "CA",
              90007
            )
          ),
          List(
            Phone(
              1,
              555888123
            ),
            Phone(
              49,
              555841332
              )
          )
        )
      )
    )
    println(addressBook.toJson)