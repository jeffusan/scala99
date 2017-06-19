package composition.d

import composition.a.Component1Trait
import composition.b.Base0Trait

trait ServiceTrait {

  def serve()(implicit component: Component1Trait, b: Base0Trait): Int

}

object Service {

  val serviceInstance = new ServiceTrait {
    def serve()(implicit c: Component1Trait, b: Base0Trait): Int = c.do1()
  }
}
