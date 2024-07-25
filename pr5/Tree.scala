import java.io.{FileInputStream, FileOutputStream, NotSerializableException, ObjectInputStream, ObjectOutputStream}


class Tree private(
                    var root: Node,
                    var comparisonCount: Integer,
                    keyToIndex: String
                  ) extends Serializable {

  def key: String = {
    keyToIndex
  }

  def height: Integer = {
    Node.height(root)
  }

  def find(value: Any): List[MapNode] = {
    comparisonCount = 0
    find(root, value).map(_.data)
  }

  private def find(node: Node, value: Any, foundNodes: List[Node] = List.empty): List[Node] = {
    if (node == null) foundNodes
    else {
      val nodeValue = node.data.get(keyToIndex)
      comparisonCount += 1
      if (value == nodeValue) {
        val leftFound = find(node.left, value, foundNodes.appended(node))
        find(node.right, value, leftFound)
      } else if (value.toString < nodeValue.toString) {
        find(node.left, value, foundNodes)
      } else {
        find(node.right, value, foundNodes)
      }
    }
  }


}

object Tree {
  def makeFrom(mapNodes: List[MapNode], key: String): Tree = {
    var root: Node = null
    for (jnode <- mapNodes) {
      if (jnode.contains(key)) {
        root = Node.insert(root, jnode, jsonNode => jsonNode.get(key).toString)
      }
    }
    new Tree(root, 0, key)
  }

  def saveIndex(index: Tree, filePath: String): Unit = {
    val fileOutputStream = new FileOutputStream(filePath)
    val objectOutputStream = new ObjectOutputStream(fileOutputStream)
    objectOutputStream.writeObject(index)
    objectOutputStream.close()
    fileOutputStream.close()
  }

  def loadIndex(filePath: String): Tree = {
    val fileInputStream = new FileInputStream(filePath)
    val objectInputStream = new ObjectInputStream(fileInputStream)
    val tree = objectInputStream.readObject()
    objectInputStream.close()
    fileInputStream.close()
    tree match {
      case tree: Tree => tree
      case _ => throw new NotSerializableException("Из данных в файле невозможно создать дерево(")
    }
  }
}
