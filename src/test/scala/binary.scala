import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class binarySpec extends Spec with ShouldMatchers {
  describe("A tree") {
    val singleton = Node("x")
    val rightDoubleton = new Node("x", End, singleton)
    val leftDoubleton = new Node("x", singleton, End)

    describe("that is empty") {
      it("should be balanced") {
        End.isBalanced should be(true)
      }
      it("should have a min/max depth of 0,0") {
        val (min, max) = End.minMax
        min should be(0)
        max should be(0)
      }
    }
    
    describe("that has one element") {
      val node = singleton
      it("should be balanced") {
        node.isBalanced should be (true)
      }
      it("should have a min/max depth of 1,1"){
        val (min, max) = node.minMax
        min should be(1)
        max should be(1)

      }
    }

    describe("with two nodes") {
      it("should be balanced"){
        Node("x", singleton, End).isBalanced should be (true)
      }
    }
    describe("a right completely unbalanced tree") {
      it("should be unbalanced") {
        Node("x", End, Node("y", End, singleton)).isBalanced should be (false)
      }
    }
    describe("a left completely unbalanced tree") {
      it("should be unbalanced") {
        Node("x", Node("y", singleton, End), End ).isBalanced should be (false)
      }
    }

    describe("when attempting to balance") {
      it("should balance a one element tree") {
        SuperTree.cBalanced(1, "x") should be(List(Node("x", End, End)))
      }

      it("should balance a two element tree") {
        SuperTree.cBalanced(2, "x") should be(List(Node("x",End, Node("x", End, End)), Node("x", Node("x", End, End), End)))
      }

      it("should balance a three element tree") {
        SuperTree.cBalanced(3, "x") should be(List(Node("x",
          Node("x", End, End),
          Node("x", End, End))))
      }

      it("should balance a four element tree") {
        val nilList:List[Tree[String]] = Nil
        nilList should contain (Node("x",singleton,leftDoubleton))
//        SuperTree.cBalanced(4, "x") should contain (Node("x",singleton,leftDoubleton))

//            ) and
//          contain (Node("x",singleton,rightDoubleton)) and
//          contain (Node("x",leftDoubleton, singleton)) and
//          contain (Node("x",rightDoubleton, singleton))
//        )
      }

      it("should be balanced") {
        val expected = List(Node("x",
          Node("x", End, End),
          Node("x",
            End,
            Node("x", End, End))))
        SuperTree.cBalanced(4, "x") should be(expected)
      }

      it("should return more than one tree option") {
        val expected = List(Node("x",
          Node("x", End, End),
          Node("x",
            End,
            Node("x", End, End))))

        val twoExpected = expected ::: List(Node("x",
          Node("x", End, End),
          Node("x",
            Node("x", End, End)),
          End))

        SuperTree.cBalanced(4, "x") should be(twoExpected)
      }


    }
  }
}