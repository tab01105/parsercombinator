package jp.ed.nnn.parsercombinator

import scala.util.parsing.combinator._

object JavaTokenParsersJSONParser extends JavaTokenParsers {

  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ { Map() ++ _ }

  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }

  def value: Parser[Any] = {
    obj |
      arr |
      stringLiteral |
      floatingPointNumber ^^ { _.toDouble } |
      "null" ^^  { _ => null } |
      "true" ^^  { _ => true } |
      "false" ^^  { _ => false}
  }

  def apply(input: String): Any = parseAll(value, input)

}
