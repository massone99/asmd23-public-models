
package u06.modelling

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import u06.exercises.ReaderWriterPN.*
import u06.utils.MultiSet
import u06.modelling.SystemAnalysis.*

class SystemAnalysisSpec extends AnyFlatSpec with Matchers:

  "cachedPaths" should "return the same results as paths" in {
    val initialMarking = MultiSet(Start, Start, Mutex)

    (1 to 10).foreach { depth =>
      val ps = readersWritersPN.paths(initialMarking, depth)
      val cps = readersWritersPN.cachedPaths(initialMarking, depth)
      cps.toList should contain theSameElementsAs ps.toList
    }
  }