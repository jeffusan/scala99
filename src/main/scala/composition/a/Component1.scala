package composition.a

import composition.b.Base0Trait


trait Component1Trait {
  def do1()(implicit base: Base0Trait): Int
}

object Component1 {


  val component1 = new Component1Trait {
    def do1()(implicit base: Base0Trait): Int = {
      base.doSome()
    }
  }

}
