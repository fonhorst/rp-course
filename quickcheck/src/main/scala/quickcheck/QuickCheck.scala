package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import java.util.Random
import GeneratorFactory._
import sun.misc.Compare

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min from two values") = {
    val a = 5
    val b = 4
    val h = insert(b, insert(a, empty))
    findMin(h) == (if (a < b) a else b) 
  }
  
  property("delete elm") = {
    val a = 5
    val initial_h = insert(a, empty)
    val post_h = deleteMin(initial_h)
    isEmpty(post_h)
  }
  
  property("simple meld") = {
    val a = 5
    val b = 4
    val a_h = insert(a, empty)
    val b_h = insert(b, empty)
    
    val melded_h = meld(a_h, b_h)
    
    findMin(melded_h) == (if (a < b) a else b) 
    
  }
  
  property("not is empty") = {
    val a = 5
    val h = insert(a, empty)
    !isEmpty(h)
  }

  property("is empty") = {
    isEmpty(empty)
  }
  
  property("ordering") = {
    val ideal = List(1,2,4,5,7,8,9)
    val h = insert(5,insert(2,insert(4, insert(7, insert(1, insert(8, insert(9, empty)))))))
    val seq = getSeq(h)
    isSortedListEqual(seq, ideal)
  }
  
  private def isSortedListEqual(l1:List[A],l2:List[A]):Boolean = (l1,l2) match {
    case (Nil,Nil) => true
    case (_,Nil) => false
    case (Nil, _) => false
    case (h1::t1,h2::t2) => (h1 equals h2) && isSortedListEqual(t1,t2)  
  }
  
  private def getSeq(ts:H):List[A] = ts match{
    case heap if isEmpty(heap) => List()
    case heap => findMin(heap)::getSeq(deleteMin(heap))
  }
  
   
  
  
  private def intH:H ={  
    val	n = bools
    val x = integers
    val h = if (n) intH else empty
    insert(x,h)
  }

  lazy val genHeap: Gen[H] = ???
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
