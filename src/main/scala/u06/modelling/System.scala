package u06.modelling

// The definition of a Rewrite System, as a function: S => Set[S]
trait System[S]:
  /** Given a state 'a', returns the set of all possible next states */
  def next(a: S): Set[S]

// Our factory of Systems
object System:

  
  /**
   * The most general case, an intensional one
   * 
   * Creates a System from a partial function.
   * This is the most flexible way to define a system, allowing for complex transition logic.
   * @param f The partial function defining the transitions
   * @return A new System instance
   */
  def ofPartialFunction[S](f: PartialFunction[S, Set[S]]): System[S] = s =>
    // If the function is defined for s, return f(s), otherwise return an empty set (as declared by the lambda _ => Set[S]())
    f.applyOrElse(s, _ => Set[S]())

  /**
   * Extensional specification
   * 
   * Creates a System from an explicit set of transitions (relation).
   * Useful when the system's behavior can be fully enumerated.
   * @param rel Set of tuples representing transitions from one state to another
   * @return A new System instance
   */
  def ofRelation[S](rel: Set[(S, S)]): System[S] = ofPartialFunction: s =>
    // Collects all possible destination states for the given source state
    rel collect:
      // The backtick operator in `s` ensures we match exactly the input state
      // Returns a Set containing all possible destination states for the given source state
      // If no transitions exist for the state, returns an empty Set (following the impl of ofPartialFunction)
      case (`s`, s2) => s2

  /**
   * Extensional with varargs
   * 
   * Convenience method to create a System from a variable number of transitions.
   * Same as ofRelation but accepts transitions as varargs for easier usage.
   * @param rel Variable number of transition pairs
   * @return A new System instance
   */
  def ofTransitions[S](rel: (S, S)*): System[S] =
    ofRelation(rel.toSet)