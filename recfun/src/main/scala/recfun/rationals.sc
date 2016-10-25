object rationals {
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  val strange = new Rational(1,0)
}

class Rational(x: Int, y: Int) {


  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if(this.less(that)) that else this

  def numer = x
  def denom = y
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer, denom)
}

rationals.x.numer

rationals.x.less(rationals.y)

rationals.x.max(rationals.y)