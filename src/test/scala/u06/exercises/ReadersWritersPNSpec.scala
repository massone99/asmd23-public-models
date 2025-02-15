package u06.exercises

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import u06.utils.MultiSet
import u06.modelling.SystemAnalysis.*
import u06.exercises.ReaderWriterPN.*

class ReadersWritersPNSpec extends AnyFlatSpec with Matchers:

  "A Readers-Writers Petri Net" should "maintain mutual exclusion" in {
    // Initial marking with 2 processes
    val initialMarking = MultiSet(P1, P1)
    
    // Get all possible paths up to length 100 [TOO MUCH TIMEEEEEEEEEEE]
    val paths = readerWriterPNet.paths(initialMarking, 100)
    
    // Check each state in each path for violations
    val violations = for
      path <- paths.toList      // For each path
      marking <- path           // For each marking in the path
      writersCount = marking(P6)  // Count writers
      readersCount = marking(P5)  // Count readers
      if writersCount > 1 || (writersCount > 0 && readersCount > 0)  // Check violations
    yield marking

    violations shouldBe empty   // No violations should be found
  }