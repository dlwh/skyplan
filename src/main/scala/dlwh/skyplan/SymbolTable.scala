package dlwh.skyplan

trait SymbolTable[@specialized(Int, Double) T] {
  def lookup(name: String):ValExpression
}

case class Environment(resources: SymbolTable[Double],
                       bindings: SymbolTable[Int],
                       locals: SymbolTable[Int])

