package composition.c

import composition.b.Base0Trait

object BaseX {

  implicit val basex = new Base0Trait {
    def doSome() = 2
  }

}
