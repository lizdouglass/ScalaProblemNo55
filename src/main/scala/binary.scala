import scala._
import scala.List


sealed abstract class Tree[+T] {
  def isBalanced : Boolean
  def minMax : (Int, Int)
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def this(value: T) = {
    this(value, End, End)
  }
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isBalanced = {
    val ((lMin, lMax),(rMin,rMax)) = (left.minMax, right.minMax)
    Math.max(lMax, rMax) - Math.min(lMin, rMin) <= 1
  }

  override def minMax = {
    val min = 1 + Math.min(left.minMax._1, right.minMax._1)
    val max = 1 + Math.max(left.minMax._2, right.minMax._2)
    (min, max)

  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isBalanced = true
  override def minMax = (0,0)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}


object SuperTree {
  def cBalanced(numberOfNodes: Int, value: String): Seq[Tree[String]] = numberOfNodes match {
    case 0 => List(End)
    case _ => {
      lazy val result = for( leftNodeCount <- 0 to numberOfNodes-1;
           left <- SuperTree.cBalanced(leftNodeCount, value);
           right <- SuperTree.cBalanced(numberOfNodes-1-leftNodeCount, value) )
        yield Node(value, left, right)
      result.filter(node => node.isBalanced).force
    }
  }

}