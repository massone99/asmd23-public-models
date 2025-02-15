package u06.modelling

// Basical analysis helpers
object SystemAnalysis:

  // Type alias: A Path is a List (because it's ordered) of states of type S
  // Used to represent sequences of states in the system
  type Path[S] = List[S]

  extension [S](system: System[S])

    // Checks if a state is in normal form (no further transitions possible)
    def normalForm(s: S): Boolean = system.next(s).isEmpty

    // Checks if a path is complete (ends in a normal form)
    def complete(p: Path[S]): Boolean = normalForm(p.last)

    /** EXECUTION EXAMPLE OF PATHS
      *
      * 1. paths(s, 3) calls paths(s, 2)
      *
      * 2. paths(s, 2) calls paths(s, 1)
      *
      * 3. paths(s, 1) returns [[s]]
      *
      * 4. paths(s, 2) processes each path from paths(s, 1): \- For [s], gets
      * next states [a, b] \- Creates [[s,a], [s,b]]
      *
      * 5. paths(s, 3) processes each path from paths(s, 2): \- For [s,a], gets
      * next states [c, d] \- For [s,b], gets next states [e] \- Creates
      * [[s,a,c], [s,a,d], [s,b,e]]
      */

    // Generates all possible paths of exactly length 'depth' starting from state 's'
    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last)
        yield path :+ next

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth: Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter (complete(_))
