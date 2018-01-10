package jp.ed.nnn.parsercombinator

abstract class MyFirstCombinator {

  sealed trait ParseResult[+T]

  case class Success[+T](value: T, next: String) extends ParseResult[T]

  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if (input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  /**
    * string parser
    *
    * @param literal 文字列
    * @return
    */
  def s(literal: String): Parser[String] = string(literal)

  def oneOf(chars: Seq[Char]): Parser[String] = input => {
    if (input.length != 0 &&
      chars.contains(input.head)) {
      Success(input.head.toString, input.tail)
    } else {
      Failure
    }
  }

  def select[T, U >: T](left: Parser[T], right: Parser[U]): Parser[U] = input => {
    left(input) match {
      case success@Success(_, _) => success
      case Failure => right(input)
    }
  }

  def combine[T, U](left: Parser[T], right: Parser[U]): Parser[(T, U)] = input => {
    left(input) match {
      case Success(value1, next1) =>
        right(next1) match {
          case Success(value2, next2) =>
            Success((value1, value2), next2)
          case Failure =>
            Failure
        }
      case Failure =>
        Failure
    }
  }

    def rep[T](parser: Parser[T]): Parser[List[T]] = input => {
      def repinner(input: String, acc: List[T]): (List[T], String) = {
        parser(input) match {
          case Success(value, next) => {
            repinner(next, acc :+ value)
          }
          case Failure => (acc, input)
        }
      }

      var acc = List[T]()
      val (acc_next, input_next) = repinner(input, acc)
      if (acc_next.length > 0)
        Success(acc_next, input_next)
      else
        Failure
    }

  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next)
      case Failure => Failure
    }
  }

}