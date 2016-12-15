
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x - y - z
x < y
x.max(z)

val n = new Rational(2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  //secondary constructor where denom assumed to be 1
  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }


  def numer = x
  def denom = y

  def unary_- = new Rational(-numer, denom)

  def < (that: Rational): Boolean = {
    numer * that.denom < that.numer * denom
  }

  def max(that: Rational) = {
    if(this < that) that else this
  }

  def + (that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def - (that: Rational) = {
    this + -that
  }

  //performing the normalization here
  //introduces arithmetic overflow *earlier*
  //it's probably better to normalize earlier.
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}