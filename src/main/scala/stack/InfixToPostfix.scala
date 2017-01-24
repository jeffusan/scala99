package stack

class InfixToPostfix(val capacity: Int) {

  import InfixToPostfix._

  val stack = new Stack(capacity)

  def transform(input: String): Either[Failure, String] = {
    if (input.length > capacity)
      Left(Failure(s"$tooLong capacity"))
    else {

      Right(input)
    }
  }

}

object InfixToPostfix {
  val lowerPrecedence = Array('+', '-')
  val higherPrecedence = Array('*', '/')
  val leftParenthesis = '('
  val rightParenthesis = ')'

  val tooLong = "input too long. Max: "
}

case class Failure(error: String)


