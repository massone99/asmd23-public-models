package u06.exercises

import u06.modelling.PetriNet.*
import u06.modelling.{PetriNet, System}
import u06.modelling.SystemAnalysis.*
import u06.utils.MultiSet

object ReaderWriterPN:

  enum Place:
    case Start, Idle, Mutex, WaitReading, Reading, WaitWriting, Writing
  export Place.*

  def readersWritersPN: System[MultiSet[Place]] = PetriNet[Place](
    // as modeled according to the slides
    MultiSet(Start) ~~> MultiSet(Idle),
    MultiSet(Idle) ~~> MultiSet(WaitReading),
    MultiSet(Idle) ~~> MultiSet(WaitWriting),
    MultiSet(WaitReading, Mutex) ~~> MultiSet(Reading, Mutex),
    MultiSet(Reading) ~~> MultiSet(Start),
    MultiSet(WaitWriting, Mutex) ~~> MultiSet(Writing) ^^^ MultiSet(Reading),
    MultiSet(Writing) ~~> MultiSet(Start, Mutex)
  ).toSystem

@main def mainReaderWriterPN(): Unit =
  import ReaderWriterPN.*
  // Initial marking with 2 processes and 1 mutex token
  println(readersWritersPN.paths(MultiSet(Start, Start, Mutex), 10).toList.mkString("\n"))