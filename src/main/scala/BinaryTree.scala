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

            // form List of (value, level)
            def loop(nodeList: List[(String, Int)], node: Tree[A]): List[(String, Int)] = node match {
                case EmptyTree(level) => List((" ", level))
                case NonEmptyTree(v, l, r, level) => loop(nodeList, l) ++ List((v.toString, level)) ++ loop(nodeList, r)
            }

            val list = loop(Nil, this)
            type Row = List[(String, Int)]

            // get values of the the current level and attach them to the values of the next level etc
            def strip(list: Row, level: Int = 0): List[Row] = list match {
                case _ if list.count(_._2 == level) == 0 => Nil
                case _ => list.map {
                    case (v, lev) if lev == level =>
                        (v, lev)
                    case (_, lev) => (" ", lev)
                } :: strip(list, level + 1)
            }

            strip(list).map(_.map(_._1).mkString(" ")).mkString("\n")
        }
    }

    case class NonEmptyTree[A](value: A, left: Tree[A], right: Tree[A], level: Int) extends Tree[A]

    case class EmptyTree(level: Int) extends Tree[Nothing]

}
