package training

class MatchingSets {

  def findMatches[A](l: List[A], f: (A,A) => Boolean): List[(A, A)] = {
    foldLeftWithTail(l, List[(A,A)]())((r,e,t) => t.foldLeft(r)((rtmp, a) => if(f(e,a)){rtmp :+ (e,a)} else rtmp))
  }

  def foldLeftWithTail[A,B](l: List[A], z: B)(f: (B,A,List[A]) => B): B = l match {
    case Nil => z
    case head :: tail => foldLeftWithTail(tail, f(z, head, tail))(f)
  }

}