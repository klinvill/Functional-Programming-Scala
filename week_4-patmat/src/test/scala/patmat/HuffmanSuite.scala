package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("Letter count of hello, world") {
    val letterCount = times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
    letterCount.foreach(e => {
      val (char, count) = e
      char match {
        case 'h' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case 'e' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case 'l' => assert(count === 3, "Expected count of " + char + " to be 3, instead got " + count)
        case 'o' => assert(count === 2, "Expected count of " + char + " to be 2, instead got " + count)
        case ',' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case ' ' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case 'w' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case 'r' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
        case 'd' => assert(count === 1, "Expected count of " + char + " to be 1, instead got " + count)
      }
    })
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton false for empty list") {
    assert(singleton(Nil) === false)
  }

  test("singleton true for list with one tree") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }

  test("singleton false for list with two trees") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list is between leaves") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('d', 7))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5), Leaf('d', 7)))
  }

  test("combine two leafs") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }

  test("until using combine and singleton") {
    val e2 = Leaf('e', 2)
    val t3 = Leaf('t', 3)
    val x4 = Leaf('x', 4)
    val d7 = Leaf('d', 7)
    val leaflist = List(e2, t3, x4, d7)
    assert(until(singleton, combine)(leaflist) === List(Fork(d7, Fork(x4, Fork(e2, t3, List('e', 't'), 5), List('x', 'e', 't'), 9), List('d', 'x', 'e', 't'), 16)))
  }

  test("CodeTree of ddxtetetxxddxddd (should be the same tree as above)") {
    val e2 = Leaf('e', 2)
    val t3 = Leaf('t', 3)
    val x4 = Leaf('x', 4)
    val d7 = Leaf('d', 7)
    assert(createCodeTree(string2Chars("ddxtetetxxddxddd")) === Fork(d7, Fork(x4, Fork(e2, t3, List('e', 't'), 5), List('x', 'e', 't'), 9), List('d', 'x', 'e', 't'), 16))
  }

  test("decode abba using t1") {
    new TestTrees {
      assert(decode(t1, List(0, 1, 1, 0)) === "abba".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("code bits") {
    val tab: CodeTable = List(('a', List(1,0,1,1)), ('b', List(0,0,0,0)), ('c', List(1,0)))
    assert(codeBits(tab)('a') === List(1,0,1,1))
    assert(codeBits(tab)('b') === List(0,0,0,0))
    assert(codeBits(tab)('c') === List(1,0))
  }

  test("code tree of t1") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("code tree of t2") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode abbdab using t2") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abbdab".toList)) === "abbdab".toList)
    }
  }
}
