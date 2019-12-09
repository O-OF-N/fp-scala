import scala.annotation.tailrec

object Sorted extends App {

    def isSorted[A](as:Array[A], ordered: (A,A)=> Boolean): Boolean = {
        
        @tailrec
        def loop(pos:Int): Boolean = {
            if(pos == as.length) true
            else if(pos == 0 || ordered(as(pos-1),as(pos))) loop(pos+1)
            else false 
        }
        loop(0)
    }

    val asc = (i:Int, j:Int) => i<j
    val desc = (i:Int, j:Int) => i>j

    println(isSorted[Int](Array[Int](1,2,3,4,5), asc))
    println(isSorted[Int](Array[Int](5,4,3,2,1), desc))
    println(isSorted[Int](Array[Int](1,3,2,4,5), asc))
    println(isSorted[Int](Array[Int](5,4,30,2,1), desc))
}