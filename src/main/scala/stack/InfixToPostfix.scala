package stack

class InfixToPostfix(val capacity: Int) {

  import InfixToPostfix._

  def transform(input: String): Either[InfixToPostfixFailure, String] = {
    if (input.length > capacity)
      Left(InfixToPostfixFailure(tooLong + capacity))
    else {

      val builder = new StringBuilder()
      val stack = new Stack[Char](capacity)

      val chars = input.toCharArray

      chars.foreach{
        c =>
          if(c == rightParenthesis) {
            //pop stack until left parenthesis

            while (stack.peek().right.get != leftParenthesis) {
              builder.append(stack.pop().right.get)
            }
            //remove left parenthesis
            stack.pop()

          } else if(c == leftParenthesis) {
            stack.push(c)

          } else if(lowerPrecedence.contains(c)) {
            while (stack.peek().isRight && higherPrecedence.contains(stack.peek().right.get)) {
              builder.append(stack.pop().right.get)
            }
            stack.push(c)
          } else if(higherPrecedence.contains(c)) {
            stack.push(c)
          } else {
            // char is an operand
            builder.append(c)
          }
      }

      while(stack.peek().isRight) {
        builder.append(stack.pop().right.get)
      }

      Right(builder.toString)
    }
  }

//  def checkAndPopUntil

}

object InfixToPostfix {
  val lowerPrecedence = Array('+', '-')
  val higherPrecedence = Array('*', '/', '^')
  val leftParenthesis = '('
  val rightParenthesis = ')'

  val tooLong = "input too long. Max: "
}

case class InfixToPostfixFailure(error: String)


