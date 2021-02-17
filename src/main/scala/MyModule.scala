import datastructures.{Branch, Leaf, Tree}

object MyModule {
  def main(args: Array[String]): Unit = {
    println(Tree.depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))

  }
}
