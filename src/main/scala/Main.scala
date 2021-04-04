object Main extends App {

  def main(): Unit = {
    val list = List(4, 2, 3, 10, 5, 1, 6, 12, 11 ,14, 13).distinct
    val tree = BinaryTree.apply(list: _*)
    print(tree)
  }

  main()
}