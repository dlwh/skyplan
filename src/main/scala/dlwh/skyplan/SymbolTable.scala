package dlwh.skyplan

import breeze.util.Index
import breeze.collection.mutable.OpenAddressHashArray

trait SymbolTable[@specialized(Int) T] {
  def lookup(name: String):Int
  def apply(i: Int):T
  def update(i: Int, v: T)
  def numSymbols: Int
}

case class Environment(resources: SymbolTable[Double],
                       bindings: SymbolTable[Int],
                       locals: SymbolTable[Int])
