package u06.utils

// A multiset datatype
trait MultiSet[A] extends (A => Int):
  def union(m: MultiSet[A]): MultiSet[A]
  def diff(m: MultiSet[A]): MultiSet[A]
  def disjoined(m: MultiSet[A]): Boolean
  def size: Int
  def matches(m: MultiSet[A]): Boolean
  def extract(m: MultiSet[A]): Option[MultiSet[A]]
  def asList: List[A]
  def asMap: Map[A,Int]
  def iterator: Iterator[A]

// Functional-style helpers/implementation
object MultiSet:
  // Factories
  def apply[A](l: A*): MultiSet[A] = new MultiSetImpl(l.toList)
  def ofList[A](l: List[A]): MultiSet[A] = new MultiSetImpl(l)
  def ofMap[A](m: Map[A,Int]): MultiSet[A] = MultiSetImpl(m)

  // Hidden reference implementation
  private case class MultiSetImpl[A](asMap: Map[A,Int]) extends MultiSet[A]:
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
      Some(this diff m) filter (_.size == size - m.size)
    override def iterator = asMap.keysIterator
    override def toString = s"{${asList.mkString("|")}}"