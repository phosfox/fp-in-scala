package chapter.two

object Exercise {
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
}
