trait Gen[A] {

  def listOf[A](a: Gen[A]): Gen[List[A]]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop



}

trait Prop {

  def check: Boolean

  def &&(p: Prop): Boolean = this.check && p.check
}
