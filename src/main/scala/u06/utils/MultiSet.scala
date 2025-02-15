package u06.utils

/** A multiset (or bag) implementation that allows elements to appear multiple times.
 *  
 * Used to model markings in Petri Nets, where tokens can be distributed across places
 * with multiple tokens allowed in the same place.
 * 
 * Example: M = {N, N, T, C} represents a marking where:
 * - Place N contains two tokens
 * - Places T and C contain one token each
 */
trait MultiSet[A] extends (A => Int):
  /** Combines this multiset with another, summing multiplicities */
  def union(m: MultiSet[A]): MultiSet[A]
  
  /** Removes elements of m from this multiset */
  def diff(m: MultiSet[A]): MultiSet[A]
  
  /** Checks if this multiset and m have no elements in common */
  def disjoined(m: MultiSet[A]): Boolean
  
  /** Returns the total number of elements counting multiplicities */
  def size: Int
  
  /** Checks if m is contained within this multiset */
  def matches(m: MultiSet[A]): Boolean
  
  /** Attempts to remove m from this multiset
   * @return Some(result) if successful, None if not possible
   */
  def extract(m: MultiSet[A]): Option[MultiSet[A]]
  
  /** @return List representation with elements repeated according to multiplicities */
  def asList: List[A]
  
  /** @return Map from elements to their multiplicities */
  def asMap: Map[A,Int]
  
  /** @return Iterator over unique elements */
  def iterator: Iterator[A]

/** Factory methods and implementation for MultiSet */
object MultiSet:
  /** Creates a MultiSet from varargs 
   * @param l The elements to include
   */
  def apply[A](l: A*): MultiSet[A] = new MultiSetImpl(l.toList)
  
  /** Creates a MultiSet from a List
   * @param l The list of elements
   */
  def ofList[A](l: List[A]): MultiSet[A] = new MultiSetImpl(l)
  
  /** Creates a MultiSet from a Map of multiplicities
   * @param m Map from elements to their counts
   */
  def ofMap[A](m: Map[A,Int]): MultiSet[A] = MultiSetImpl(m)

  /** Private implementation using a Map to store multiplicities */
  private case class MultiSetImpl[A](asMap: Map[A,Int]) extends MultiSet[A]:
    /** Converts a list to a multiset by counting occurrences */
    def this(list: List[A]) = this(list.groupBy(a => a).map((a,n) => (a, n.size)))
    
    override val asList =
      asMap.toList.flatMap((a,n) => List.fill(n)(a))

    override def apply(v1: A) = asMap.getOrElse(v1,0)
    override def union(m: MultiSet[A]) = new MultiSetImpl[A](asList ++ m.asList)
    override def diff(m: MultiSet[A]) = new MultiSetImpl[A](asList diff m.asList)
    override def disjoined(m: MultiSet[A]) = (asList intersect m.asList).isEmpty
    override def size = asList.size
    override def matches(m: MultiSet[A]) = extract(m).isDefined
    override def extract(m: MultiSet[A]) =
      // filter keeps the result only if size decreased by exactly m.size
      Some(this diff m) filter (_.size == size - m.size)
    override def iterator = asMap.keysIterator
    override def toString = s"{${asList.mkString("|")}}"