package u06.exercises

import u06.modelling.PetriNet.*
import u06.modelling.{PetriNet, System}
import u06.modelling.SystemAnalysis.*
import u06.utils.MultiSet

object ReaderWriterPN:

  enum Place:
    case P1, P2, P3, P4, P5, P6, P7
  
  export Place.*

  def readerWriterPNet: System[MultiSet[Place]] = PetriNet[Place](
    // t1
    MultiSet(P1) ~~> MultiSet(P2),
    // t2
    MultiSet(P2) ~~> MultiSet(P3),
    // t3
    MultiSet(P2) ~~> MultiSet(P4),
    // t4
    MultiSet(P3) ~~> MultiSet(P5),
    // t5
    (MultiSet(P4) ~~> MultiSet(P7)) ^^^ MultiSet(P6), // I can go to P7 only if P6 is empty
    // t6
    MultiSet(P6) ~~> MultiSet(P1),
    // t7
    MultiSet(P7) ~~> MultiSet(P1, P5)
  ).toSystem


@main def mainReaderWriterPN(): Unit =
  import ReaderWriterPN.*
  println(readerWriterPNet.paths(MultiSet(P1, P1), 10).toList.mkString("\n"))