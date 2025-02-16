
package u06.exercises

trait SafetyProperty[S]:
  def satisfies(s: S): Boolean

  /**
    * A mutual exclusion property for readers and writers
    *
    * @param writerCount is a function that returns the number of writers in the system
    * @param readerCount is a function that returns the number of readers in the system
    */
case class MutualExclusionRW[S](writerCount: S => Int, readerCount: S => Int) extends SafetyProperty[S]:
  override def satisfies(s: S): Boolean =
    val w = writerCount(s)
    val r = readerCount(s)
    !(w > 1 || (w > 0 && r > 0))

extension [S](s: S)
  infix def satisfies(prop: SafetyProperty[S]): Boolean = prop.satisfies(s)