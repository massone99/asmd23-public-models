package u06.modelling

import u06.utils.MultiSet

object PetriNet:
  // pre-conditions, effects, inhibition
  case class Transition[P](cond: MultiSet[P], eff: MultiSet[P], inh: MultiSet[P])
  type PetriNet[P] = Set[Transition[P]]
  type Marking[P] = MultiSet[P]

  // factory of A Petri Net
  def apply[P](transitions: Transition[P]*): PetriNet[P] = transitions.toSet

  // factory of a System, as a toSystem method
  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      for
        Transition(cond, eff, inh) <- pn // get any transition
        if m disjoined inh // check inhibition
        out <- m extract cond       // remove precondition
      yield out union eff           // add effect

  // fancy syntax to create transition rules
  extension [P](self: Marking[P])
    def ~~> (y: Marking[P]): Transition[P] = Transition(self, y, MultiSet())
  extension [P](self: Transition[P])
    def ^^^ (z: Marking[P]): Transition[P] = self.copy(inh = z)