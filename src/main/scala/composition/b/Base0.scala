package composition.b


trait Base0Trait {
  def doSome(): Int
}

object Base0 {

  implicit val base = new Base0Trait {
    def doSome(): Int = 0
  }
}
