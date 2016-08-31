import scala.util.matching.Regex

object MyTests {

  def main(args: Array[String]) {
    val numericIdRegex: Regex = "[0-9]{4,}".r
//  val numericIdRegex: Regex = "[0,9]{4,}".r
//  val numericIdRegex: Regex = "[0,9]{4,}".r

    val anonymizedUri = numericIdRegex.replaceAllIn("/identities/1363047703/email", "***")

    println(anonymizedUri)

    println("/identities//test".replace("/", "_"))
  }

}
