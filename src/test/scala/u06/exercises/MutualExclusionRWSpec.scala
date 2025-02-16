package u06.exercises

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Extension to use 'satisfies' as an infix operator.
extension [S](s: S)
  def satisfies(prop: SafetyProperty[S]): Boolean = prop.holdsFor(s)

class MutualExclusionRWSpec extends AnyFlatSpec with Matchers:

  // A simple state with writer and reader counts.
  case class TestState(writers: Int, readers: Int)

  // Functions to extract the counts.
  val writerCount: TestState => Int = _.writers
  val readerCount: TestState => Int = _.readers

  // Create an instance of the mutual exclusion property.
  val mutualExclusion = MutualExclusionRW[TestState](writerCount, readerCount)

  "MutualExclusionRW" should "hold when there is no conflict" in {
    TestState(0, 0) satisfies mutualExclusion shouldBe true
    TestState(1, 0) satisfies mutualExclusion shouldBe true
    TestState(0, 3) satisfies mutualExclusion shouldBe true
  }

  it should "violate when there are multiple writers" in {
    TestState(2, 0) satisfies mutualExclusion shouldBe false
    TestState(3, 1) satisfies mutualExclusion shouldBe false
  }

  it should "violate when there is a writer and any reader concurrently" in {
    TestState(1, 1) satisfies mutualExclusion shouldBe false
    TestState(1, 5) satisfies mutualExclusion shouldBe false
  }