package advancedScala.classesAndInterfaces

object CKAnimals {

  case class Dog()

  case class Cat()

  case class Human()

  trait Talks[A] {
    def talk(x: A): String
  }

  implicit val dogTalks = new Talks[Dog] {
    def talk(x: Dog): String = "gav gav"
  }

  implicit val catTalks = new Talks[Cat] {
    def talk(x: Cat): String = "niao niao"
  }

  implicit val humanTalks = new Talks[Human] {
    def talk(x: Human): String = "awesome dude"
  }

  implicit class TalksSyntax[A](value: A)(implicit val ev: Talks[A]) {
    def talk: String = ev.talk(value)
  }

  def listTalk[A <: TalksSyntax[_]](l: List[A]): List[String] =
    l.map(i => {
      val value = i.talk
      println(value)
      value
    })


  def main(args: Array[String]) {
    println("talking one by one")
    val h = Human()
    println(h.talk)
    val c = Cat()
    println(c.talk)
    val d = Dog()
    println(d.talk)

    //TODO make this work
    println("\nTalking from a `List`")
    val lst: List[TalksSyntax[_]] = List(h, c, d)
    lst.map(i => println(i.talk))

    println("\ntalking from `listTalk`")
    listTalk(lst)
  }

}
