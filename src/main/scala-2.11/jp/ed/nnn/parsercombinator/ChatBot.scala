package jp.ed.nnn.parsercombinator

import scala.util.matching.Regex

case class ChatBot(commands: List[Command])

sealed trait Command {
  def exec(input :String): Boolean
}

case class ReplyCommand(regex: Regex, replays: List[String]) extends Command {
  override def exec(input :String): Boolean = ???
}