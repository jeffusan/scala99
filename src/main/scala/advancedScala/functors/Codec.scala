package advancedScala.functors


trait Codec[A] {

  def encode(value: A): String

  def decode(value: String): Option[A]

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String = {
        self.encode(enc(value))
      }

      def decode(value: String): Option[B] =
        self.decode(value).map(dec)
    }
  }
}



object Codec {

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)

}
