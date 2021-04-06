import scala.collection.immutable.Map

object MyGraph {

    case class Graph(nodes: Map[Int, List[Int]]){

        def addNode(nodeId: Int, neighboors: List[Int]): Option[Graph] = {
            nodes.get(nodeId) match {
                case Some(_) => None
                case None => Some(Graph(nodes + (nodeId -> neighboors)))
            }
        }

        def getNode(nodeId: Int) : Option[List[Int]] = {
            this.nodes.get(nodeId)
        }
    }

}
