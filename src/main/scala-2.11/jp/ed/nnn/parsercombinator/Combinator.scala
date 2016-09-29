package jp.ed.nnn.parsercombinator

abstract class Combinator {

  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  /**
    * string parser
    * @param literal 文字列
    * @return
    */
  def s(literal: String): Parser[String] = string(literal)

  implicit class RichParser[T](val parser: Parser[T]) {

    /**
      * select
      *
      * @param right 選択を行うパーサー
      * @return
      */
    def |[U >: T](right: => Parser[U]): Parser[U] = input => {
      parser(input) match {
        case success@Success(_, _) => success
        case Failure => right(input)
      }
    }

  }

}
