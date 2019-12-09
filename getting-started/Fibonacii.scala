import scala.annotation.tailrec

object Fibonacii extends App {

    def fib(n:Int):Int = {
        
        @tailrec
        def loop(i:Int, j:Int, currPos:Int): Int = {
            if(currPos == n) return j
            loop(j, i+j, currPos + 1)
        }
        loop(0,1, 2)
    }

    println(fib(10))
}