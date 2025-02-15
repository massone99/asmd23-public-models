package u06.examples

export u06.modelling.PetriNet
import u06.utils.MultiSet
import u06.modelling.System
object PNMutualExclusion:

  enum Place:
    case N, T, C
    
  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MultiSet

  // DSL-like specification of a Petri Net
  def pnME: System[MultiSet[Place]] = PetriNet[Place](
    MultiSet(N) ~~> MultiSet(T),
    MultiSet(T) ~~> MultiSet(C) ^^^ MultiSet(C),
    MultiSet(C) ~~> MultiSet()
  ).toSystem

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*
  // example usage
  println(pnME.paths(MultiSet(N,N),7).toList.mkString("\n"))
