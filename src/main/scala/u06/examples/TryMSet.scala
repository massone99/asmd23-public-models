package u06.examples

import u06.utils.MultiSet

// Some simple client code.. also check MSetSpec and MSetCheck
object TryMSet extends App:
  import MultiSet.*

  val m1 = MultiSet(10,20,30,30,40,40,50)
  val m2: MultiSet[Int] = MultiSet(50,10,20,30,30,40,40)
  println(m1)
  println(m1 == m2)
  println(m1.asList)
  println(m1.asMap)
  println(m1.iterator.toList)
  println(m1.extract(MultiSet(50,10)))
