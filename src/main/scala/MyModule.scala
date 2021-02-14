import chapter.two.Exercise._

object MyModule {
  def main(args: Array[String]): Unit =
    println(factorial(5))
    println(fib(6))
    println(formatResult("factorial", 7, factorial))
    println(isSorted(Array(1,2,3,4), (a: Int, b: Int)  => a < b))
    println(isSorted(Array(), (a: Int, b: Int)  => a > b))
}
