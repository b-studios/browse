case class B(xyz: Int)
trait Q {
  def m(i: Int) = i
  def m2(a: Int, b: Int) = a + b
  def self = this
  
  def accessingCaseClassMethods = {
    B(42).copy()
  }
}

