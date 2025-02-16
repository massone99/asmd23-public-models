package u06.exercises

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import u06.utils.MultiSet
import u06.modelling.SystemAnalysis.*
import u06.exercises.ReaderWriterPN.*

class ReadersWritersPNSpec extends AnyFlatSpec with Matchers:

  "A Readers-Writers Petri Net" should "maintain mutual exclusion" in {
    // Initial marking with 2 processes and 1 mutex token
    val initialMarking = MultiSet(Start, Start, Mutex)
    
    // Get all possible paths up to length 100
    val paths = readersWritersPN.cachedPaths(initialMarking, 100)
    
    // Check each state in each path for violations
    val violations = for
      path <- paths.toList
      marking <- path
      // the apply of marking returns the number of tokens in the place given as input
      writersCount = marking(Writing)    // Count writers
      readersCount = marking(Reading)    // Count readers
      if writersCount > 1 || (writersCount > 0 && readersCount > 0)  // Check violations
    yield marking

    violations shouldBe empty   // No violations should be found
  }

  it should "allow multiple readers simultaneously" in {
    val initialMarking = MultiSet(Start, Start, Mutex)
    val paths = readersWritersPN.paths(initialMarking, 10)
    
    // Check if there's at least one state with multiple readers
    val multipleReaders = paths.toList.exists(path => 
      path.exists(marking => marking(Reading) > 1)
    )
    
    multipleReaders shouldBe true
  }