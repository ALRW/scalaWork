package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times: returns occurance of each letter") {
    val chars: List[Char] = List('h', 'e', 'l', 'l', 'o')
    assert(times(chars) === List(('h', 1), ('e', 1), ('l', 2), ('o', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton: checks if object is singleton"){
    new TestTrees {
      val goodList = List[CodeTree](t1)
      val badList = List[CodeTree](t1, t2)
      val emptyList = List[CodeTree]()
      assert(singleton(goodList))
      assert(!singleton(badList))
      assert(!singleton(emptyList))
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until: creates final singleton tree"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist).length === 1)
  }

  test("createCodeTree: make a code tree"){
    val chars: List[Char] = List('h', 'e')
    assert(createCodeTree(chars) === Fork(Leaf('h',1),Leaf('e',1),List('h', 'e'),2))
  }

  test("decode: uses a tree to decode a sequence of bits"){
    new TestTrees {
      val bits: List[Bit] = List(0)
      assert(decode(t1, bits) === List[Char]('a'))
    }
  }

  test("encode: encodes list of chars"){
    new TestTrees {
      val chars = List('b', 'a','d')
      assert(encode(t2)(chars) === List[Bit](0,1,0,0,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits: bitsequence for a char in codeTable"){
    val table = List(('b', List(0,1)), ('a', List(0,0)), ('d', List(1)))
    assert(codeBits(table)('a') == List(0,0))
  }

  test("convert: given a tree returns a code-table"){
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)), ('b',List(1))))
    }
  }

  test("mergeCodeTables: concat's two CodeTables"){
    val c1: CodeTable = List(('a', List(0)), ('b', List(1)))
    val c2: CodeTable = List(('c', List(0)), ('d', List(1)))
    assert(mergeCodeTables(c1, c2) === c1 ::: c2)
  }

  test("quickEncode: encodes list using codeTree"){
    assert(quickEncode(frenchCode)(List('h','u','f','f','m','a','n','e','s','t','c','o','o','l')) === secret)
  }

}
