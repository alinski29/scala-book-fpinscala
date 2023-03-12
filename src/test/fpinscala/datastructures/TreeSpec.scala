package fpinscala.datastructures

import fpinscala.TestSpec
import fpinscala.datastructures.Tree
import fpinscala.datastructures.Tree.*

class TreeSpec extends TestSpec {

  "Tree data structure" should {

    val treeStr = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )

    val treeInt = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(Leaf(3), Leaf(4))
    )

    "have correct implementations" in {
      treeStr.size shouldEqual 7
      treeInt.firstPositive shouldEqual 1
      treeInt.maximum shouldEqual 4
      treeStr.depth shouldEqual 2
      treeInt.map(_ + 1) shouldEqual Branch(
        Branch(Leaf(2), Leaf(3)),
        Branch(Leaf(4), Leaf(5))
      )
    }

    "implement fold and functions via fold" in {
      treeStr.sizeViaFold shouldEqual 7
      treeInt.sizeViaFold shouldEqual 7
      treeInt.maximumViaFold shouldEqual 4
      treeStr.depthViaFold shouldEqual 2
      treeInt.mapViaFold(_ + 1) shouldEqual Branch(
        Branch(Leaf(2), Leaf(3)),
        Branch(Leaf(4), Leaf(5))
      )
    }

  }
}
