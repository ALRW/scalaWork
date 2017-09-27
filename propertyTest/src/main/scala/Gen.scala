import Prop.{FailedCase, SuccessCount}

trait Gen[A] {

  def listOf[A](a: Gen[A]): Gen[List[A]]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop



}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}
trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

}
