package chapter.two

object Exercise {
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        a => (b => f(a, b))
    }
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }
        
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = {
            if (n >= as.length - 1) true
            else if (ordered(as(n), as(n+1))) loop(n+1)
            else false
        }
        loop(0)
    }

    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int = 
            if (n <= 0) acc
            else go(n-1, n*acc)
    go(n, 1)
    }

    def fib(n: Int): Int = {
        def go(n: Int, a: Int, b: Int): Int = 
            n match {
                case 0 => a
                case _ => go(n-1, b, a+b)
        }
        go(n, 0, 1)
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }
}
