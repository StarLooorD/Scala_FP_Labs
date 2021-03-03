package lab4

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HuffmanSpec extends AnyFreeSpec with Matchers {

  import Huffman._

  val a1 = Leaf('a', 1)
  val b4 = Leaf('b', 4)
  val c6 = Leaf('c', 6)
  val d8 = Leaf('d', 8)

  val orderedLeafs = List(a1, b4, c6, d8)
  val testTree = combine(orderedLeafs).head

  val ab4 = Fork(a1, b4, List('a', 'b'), 5)

  "weight" in {
    weight(a1) shouldBe 1
    weight(ab4) shouldBe 5
  }

  "chars" in {
    chars(a1) shouldBe List('a')
    chars(ab4) shouldBe List('a', 'b')
  }

  "makeCodeTree" in {
    makeCodeTree(a1, b4) shouldBe ab4
  }

  "strToChars" in {
    strToChars("ab") shouldBe List('a', 'b')
  }

  "times & ordered" - {
    val testCharsTimes = List('a' -> 4, 'b' -> 3, 'c' -> 2, 'f' -> 1)

    "times" in {
      times(strToChars("abcabacfba")) should contain theSameElementsAs testCharsTimes
    }

    "makeOrderedLeafList" in {
      makeOrderedLeafList(List('a' -> 4, 'c' -> 2, 'b' -> 3, 'f' -> 1)) shouldBe
        testCharsTimes.view.map { case (c, w) => Leaf(c, w) }.reverse.toList
    }
  }

  "insert" in {
    insert(a1, List(b4, c6, d8)) shouldBe orderedLeafs
    insert(b4, List(a1, c6, d8)) shouldBe orderedLeafs
    insert(c6, List(a1, b4, d8)) shouldBe orderedLeafs
    insert(d8, List(a1, b4, c6)) shouldBe orderedLeafs
  }

  "combine" in {
    testTree shouldBe makeCodeTree(d8, makeCodeTree(makeCodeTree(a1, b4), c6))
  }

  "createCodeTree" in {
    val chars = orderedLeafs.flatMap(l => List.fill(l.weight)(l.char))
    createCodeTree(chars) shouldBe testTree
  }

  val helloTree = Fork(Leaf('l', 4), Fork(Fork(Leaf('o', 1), Leaf('e', 2), List('o', 'e'), 3), Leaf('h', 3), List('o', 'e', 'h'), 6), List('l', 'o', 'e', 'h'), 10)

  "decode" in {
    decode(helloTree, List(1, 1, 1, 0, 1, 0, 0, 1, 0, 0)) shouldBe "hello".toList
  }

  "decodedSecret" in {
    decodedSecret shouldBe "huffmanestcool".toList
  }

  "encode" in {
    val encoder = encode(helloTree) _
    encoder("hello".toList) shouldBe List(1, 1, 1, 0, 1, 0, 0, 1, 0, 0)
    encoder("elo".toList) shouldBe List(1, 0, 1, 0, 1, 0, 0)
    encode(frenchCode)("huffmanestcool".toList) shouldBe secret
  }

  val helloTable = List(
    'h' -> List(1, 1),
    'e' -> List(1, 0, 1),
    'l' -> List(0),
    'o' -> List(1, 0, 0),
  )

  "codeBits" in {
    codeBits(helloTable)('h') shouldBe List(1, 1)
    codeBits(helloTable)('l') shouldBe List(0)
  }

  "convert" in {
    convert(helloTree) should contain theSameElementsAs helloTable
  }

  "quickEncode" in {
    val encoder = quickEncode(helloTree) _
    encoder("hello".toList) shouldBe List(1, 1, 1, 0, 1, 0, 0, 1, 0, 0)
    encoder("elo".toList) shouldBe List(1, 0, 1, 0, 1, 0, 0)
    quickEncode(frenchCode)("huffmanestcool".toList) shouldBe secret
  }
}
