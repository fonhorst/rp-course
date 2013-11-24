package quickcheck

import java.util.Random

object GeneratorFactory{
  lazy val intGen = new IntGenerator
  lazy val boolGen = new BoolGenerator 
  def integers = intGen.generate
  def bools = boolGen.generate
}

trait Generator[T]{
  def generate:T
}

class IntGenerator extends Generator[Int] {
	val rand = new Random
	override def generate:Int = rand.nextInt()
}

class BoolGenerator extends Generator[Boolean]{
	val rand = new Random
	override def generate:Boolean = rand.nextBoolean()
} 