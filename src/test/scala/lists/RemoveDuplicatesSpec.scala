package lists

import lists.model.Item
import lists.model.Kind._
import org.scalatest.funsuite.AnyFunSuite

class RemoveDuplicatesSpec extends AnyFunSuite {

  test("lists without duplicates") {
    val ls = List(
      Item(1, O),
      Item(2, O),
      Item(3, B)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    assert(response == ls)
  }
  test("lists with id duplicate but type distinct without rerank") {
    val ls = List(
      Item(1, O),
      Item(2, O),
      Item(2, B),
      Item(3, B)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(2, O),
      Item(3, B)
    )
    assert(response == expected)
  }
  test("lists with several elements duplicates. no rerank needed") {
    val ls = List(
      Item(1, O),
      Item(2, O),
      Item(2, B),
      Item(3, B),
      Item(4, O),
      Item(5, B),
      Item(3, O)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(2, O),
      Item(3, B),
      Item(4, O),
      Item(5, B)
    )
    assert(response == expected)
  }
  test("lists with several elements duplicates and one element appears many times. no rerank needed") {
    val ls = List(
      Item(1, O),
      Item(2, O),
      Item(2, B),
      Item(3, B),
      Item(4, O),
      Item(3, B),
      Item(5, B),
      Item(3, O),
      Item(3, B)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(2, O),
      Item(3, B),
      Item(4, O),
      Item(5, B)
    )
    assert(response == expected)
  }
  test("lists with one element repeated. Rerank is needed") {
    val ls = List(
      Item(1, O),
      Item(1, O),
      Item(2, B),
      Item(3, O)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(3, O),
      Item(2, B)
    )
    assert(response == expected)
  }

  test("lists with several elements repeated. Rerank is needed") {
    val ls = List(
      Item(1, O),
      Item(2, B),
      Item(1, B),
      Item(3, O),
      Item(4, B),
      Item(4, O),
      Item(5, O),
      Item(6, O),
      Item(7, B),
      Item(5, O)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(2, B),
      Item(4, B),
      Item(3, O),
      Item(5, O),
      Item(6, O),
      Item(7, B),
    )
    assert(response == expected)

    /* step by step
    step 1
    Item(1, Organic)                     Item(1, Organic)                      Item(1, Organic)                       Item(1, Organic)                    Item(1, Organic)
    Item(2, Boosted)                     Item(2, Boosted)                      Item(2, Boosted)                       Item(2, Boosted)                    Item(2, Boosted)
    Item(1, Boosted) <- delete this one  find boosted to replace it            Item(4, Boosted)                       Item(4, Boosted)                    Item(4, Boosted)
    Item(3, Organic)                     Item(3, Organic)                      Item(3, Organic)                       Item(3, Organic)                    Item(3, Organic)
    Item(4, Boosted)                     Item(4, Boosted) <- replace with this Item(4, Organic) <- delete this one    Item(5, Organic)                    Item(5, Organic)
    Item(4, Organic)                     Item(4, Organic)                      Item(5, Organic) <- replace with this  Item(6, Organic)                    Item(6, Organic)
    Item(5, Organic)                     Item(5, Organic)                      Item(6, Organic)                       Item(7, Boosted)                    Item(7, Boosted)
    Item(6, Organic)                     Item(6, Organic)                      Item(7, Boosted)                       Item(5, Organic) <- delete this one
    Item(7, Boosted)                     Item(7, Boosted)                      Item(5, Organic)
    Item(5, Organic)                     Item(5, Organic)
     */
  }

  test("lists with several elements repeated and the number or appears is > 1. Rerank is needed") {
    val ls = List(
      Item(1, O),
      Item(2, B),
      Item(1, B),
      Item(3, O),
      Item(4, B),
      Item(4, O),
      Item(1, B),
      Item(5, O),
      Item(6, O),
      Item(7, B),
      Item(5, O)
    )
    val response = new RemoveDuplicates().removeDuplicatesAndRerank(ls)
    val expected = List(
      Item(1, O),
      Item(2, B),
      Item(4, B),
      Item(3, O),
      Item(5, O),
      Item(7, B),
      Item(6, O),
    )
    assert(response == expected)
  }
}
