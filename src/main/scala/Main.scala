import scala.util.Random

object Main extends App {

  def main(): Unit = {
    val list = List(4, 2, 3, 10, 5, 1, 6, 12, 11 ,14, 13).distinct
    val list2 = Seq.fill(32)(elem = (Random.nextDouble()*100).toInt).toList.distinct
    //println(list2)
    val tree = BinaryTree.apply(list2: _*)
    print(tree)
  }

  main()
}