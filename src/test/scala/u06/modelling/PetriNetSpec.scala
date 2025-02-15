package u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class PetriNetSpec extends AnyFunSuite:

  import u06.examples.PNMutualExclusion.*

  test("PN for mutual exclusion should properly generate 7-length paths"):

    val expected1 = List(MultiSet(N,N), MultiSet(T,N), MultiSet(T,T), MultiSet(C,T), MultiSet(T), MultiSet(C), MultiSet())
    val expected2 = List(MultiSet(N,N), MultiSet(T,N), MultiSet(C,N), MultiSet(C,T), MultiSet(T), MultiSet(C), MultiSet())
    val expected3 = List(MultiSet(N,N), MultiSet(T,N), MultiSet(C,N), MultiSet(N), MultiSet(T), MultiSet(C), MultiSet())

    pnME.paths(MultiSet(N,N),7).toSet should be:
      Set(expected1, expected2, expected3)
