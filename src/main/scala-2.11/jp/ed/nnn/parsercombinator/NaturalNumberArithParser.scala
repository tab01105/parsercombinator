package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

object NaturalNumberArithParser  extends MyFirstCombinator{

  def digitExcludingZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitExcludingZero)

  def naturalNumber: Parser[String] = map(combine(digitExcludingZero, rep(digit)), {
    t: (String, List[String]) => t._2.foldLeft(t._1) { (acc, str) => acc + str }
  })

  def toInt(str:String):Int = str.foldLeft(0) { (acc, c) => acc * 10 + c.toInt - '0'.toInt }

  def apply(input: String): ParseResult[NaturalNumberArith] = map(combine(naturalNumber,
    rep(map(combine(select(s("+"), s("-")), naturalNumber), {
      t: (String, String) => (t._1, toInt(t._2))
    }))), {
    t: (String, List[(String, Int)]) => NaturalNumberArith(toInt(t._1), t._2)
  })(input)
}
