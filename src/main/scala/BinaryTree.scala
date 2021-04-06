import scala.annotation.tailrec

object BinaryTree {

    def apply[A](elements: A*)(implicit n: Numeric[A]): Tree[A] = {
        elements.foldLeft(EmptyTree(0): Tree[A])((tree, item) => tree.add(item))
    }

    sealed trait Tree[+A] {

        def add[B >: A](value: B)(implicit n: Numeric[B]): Tree[B] = {
            import n._
            this match {
                case EmptyTree(lev) => NonEmptyTree(value, EmptyTree(lev + 1), EmptyTree(lev + 1), lev)
                case NonEmptyTree(v, l, r, lev) => v match {
                    case v if value > v => NonEmptyTree(v, l, r.add(value), lev)
                    case v if value < v => NonEmptyTree(v, l.add(value), r, lev)
                }
            }
        }

        override def toString: String = {
            type Row = List[(String, Int)]
            // form List of (value, level)
            def loop(nodeList: Row, node: Tree[A]): Row = node match {
                case EmptyTree(level) => List(("-", level))
                case NonEmptyTree(v, l, r, level) => loop(nodeList, l) ++ List((v.toString, level)) ++ loop(nodeList, r)
            }

            val list = loop(Nil, this)


            // get values of the the current level and attach them to the values of the next level etc
            @tailrec
            def strip(list: Row, level: Int = 0, buffer: List[Row]=List.empty[Row]): List[Row] = level match {
                case _ if list.count(x => (x._1 != " ") && (x._2 == level)) == 0 => buffer
                case _ => strip(list, level + 1, buffer :+ list.map {
                    case (v, lev) if lev == level => (v, lev)
                    case (_, lev) => (" ", lev)}
                )
            }
            strip(list).map(_.map(_._1).mkString(" ")).mkString("\n")
        }
    }

    case class NonEmptyTree[A](value: A, left: Tree[A], right: Tree[A], level: Int) extends Tree[A]

    case class EmptyTree(level: Int) extends Tree[Nothing]

}
