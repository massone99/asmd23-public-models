package u06.utils

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

object MSetCheck extends Properties("MSet"):

  given msetArbitrary[A:Arbitrary]: Arbitrary[MultiSet[A]] =
    Arbitrary(arbitrary[List[A]] map (MultiSet.ofList(_)))

  property ("has constructors that are compatible") = forAll: (list: List[Int]) =>
    val m1 = MultiSet.ofList(list); m1 == MultiSet.ofMap(m1.asMap)

  property ("is ordering independent") = forAll: (list: List[Int]) =>
    MultiSet.ofList(list) == MultiSet.ofList(scala.util.Random.shuffle(list))

  property ("is duplicate dependent") = forAll: (list: List[Int], i:Int) =>
    MultiSet.ofList(i :: list) != MultiSet.ofList(i :: i :: list)

  property ("has union semantics that corresponds to List's") = forAll: (list1: List[Int], list2: List[Int]) =>
    MultiSet.ofList(list1 concat list2) == MultiSet.ofList(list1).union(MultiSet.ofList(list2))

  property ("has diff semantics that corresponds to List's") = forAll: (list1: List[Int], list2: List[Int]) =>
    MultiSet.ofList(list1 diff list2) == MultiSet.ofList(list1).diff(MultiSet.ofList(list2))

  property ("has size with semantics that corresponds to List's") = forAll: (list1: List[Int]) =>
    MultiSet.ofList(list1).size == list1.size

  property ("has matches that is coherent with diff") = forAll: (mset1: MultiSet[Int], mset2: MultiSet[Int]) =>
    mset1 matches (mset1 diff mset2)

  property ("has extract that is coherent with union") = forAll: (mset1: MultiSet[Int], mset2: MultiSet[Int]) =>
    val mset3 = mset1 diff mset2; (mset1 extract mset3).get.union(mset3) == mset1

  property ("has iterator that gives values in MSet") = forAll: (mset: MultiSet[Int]) =>
    mset.iterator forall (mset matches MultiSet(_))

  property ("has iterator that does not miss values in MSet") = forAll: (list: List[Int]) =>
    val elements = MultiSet.ofList(list).iterator.toList; list forall (elements contains _)
