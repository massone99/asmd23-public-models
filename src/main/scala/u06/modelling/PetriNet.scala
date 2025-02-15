package u06.modelling

import u06.utils.MultiSet

object PetriNet:
  /** A Transition in a Petri Net consists of:
   * @param cond Pre-conditions (tokens that must be present)
   * @param eff Effects (tokens produced by firing)
   * @param inh Inhibitor arcs (tokens that prevent firing)
   *    In Petri Nets, inhibitor arcs prevent a transition from firing 
   *    when certain places contain tokens. It's a marking that prevents this transition from happening
   */
  case class Transition[P](cond: MultiSet[P], eff: MultiSet[P], inh: MultiSet[P])
  // The type PetriNet[P] is defined as a Set of Transitions
  type PetriNet[P] = Set[Transition[P]]
  // The type Marking[P] is defined as a MultiSet
  type Marking[P] = MultiSet[P]

  // Why P? Because a state can be modeled using any type (integer, string, etc.) or class
  // See TryMSet.scala for an example of usage

  // factory of A Petri Net
  def apply[P](transitions: Transition[P]*): PetriNet[P] = transitions.toSet

  // Defining extension methods for PetriNet
  extension [P](pn: PetriNet[P])
    // factory of a System, as a toSystem method
    def toSystem: System[Marking[P]] = marking =>
      for
        // <- is the generator operator (similar to in in other languages)
        // like writing: for transition in pn
        Transition(precondition, markingProducedByTransition, inhbitorMarking) <- pn // Get a transition with its inhibitors
        // m disjoined inh returns true if the two multisets have no elements in common
        // m and inh are both Marking[P] (MultiSet[P])
        if marking disjoined inhbitorMarking
        // like writing: for out in m
        // m extract cond => rimuove da m tutti gli elementi presenti in cond
        // considerando che cond è il marking necessario per far scattare la transizione
        // ciò significa rimuovere i token necessari per far scattare la transizione
        // (i.e. pagare il costo!)

        // out rappresenta il marking originale meno i token necessari per far scattare la transizione
        // che sono stati rimossi con m extract cond
        markingAfterConsumingCond <- marking extract precondition                // If check passes, try to consume input tokens
      yield markingAfterConsumingCond union markingProducedByTransition                    // If successful, produce output tokens


  // Defining extension methods for Marking
  extension [P](self: Marking[P])
    /** Syntactic sugar for creating basic transitions with no inhibitor arcs
     * Enables writing transitions as: place1 ~~> place2
     * @return A new Transition with no inhibitor arcs
     */
    def ~~> (y: Marking[P]): Transition[P] = Transition(self, y, MultiSet())
  
  // Defining extension methods for Transition
  extension [P](self: Transition[P])
    /** Syntactic sugar for adding inhibitor arcs
     * Enables writing transitions as: (place1 ~~> place2) ^^^ place3
     * @return A new Transition with the specified inhibitor arc
     */
    def ^^^ (z: Marking[P]): Transition[P] = 
      // self.copy(inh = z): Creates a new transition copying all fields from self but replacing the inh field with z
      self.copy(inh = z)