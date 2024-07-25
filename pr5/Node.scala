
class Node(
            var data: MapNode,
            private var height: Integer = 1,
            var left: Node = null,
            var right: Node = null
          ) extends Serializable {


  def balanceFactor: Integer = {
    Node.height(right) - Node.height(left)
  }

  def fixHeight(): Unit = {
    height = Math.max(Node.height(left), Node.height(right)) + 1
  }

  override def toString: String = data.toString

}

object Node {
  def height(node: Node): Integer = {
    if (node != null) node.height else 0
  }


  private def rotateRight(p: Node): Node = {
    val q = p.left
    p.left = q.right
    q.right = p
    p.fixHeight()
    q.fixHeight()
    q
  }

  private def rotateLeft(q: Node): Node = {
    val p = q.right
    q.right = p.left
    p.left = q
    q.fixHeight()
    p.fixHeight()
    p
  }

  private def balance(p: Node): Node = {
    p.fixHeight()

    if (p.balanceFactor == 2) {
      if (p.right.balanceFactor < 0) {
        p.right = rotateRight(p.right)
      }
      return rotateLeft(p)
    }

    if (p.balanceFactor == -2) {
      if (p.left.balanceFactor > 0) {
        p.left = rotateLeft(p.left)
      }
      return rotateRight(p)
    }

    p
  }

  def insert(p: Node, value: MapNode, key: MapNode => String): Node = {
    if (p == null) return new Node(value)
    if (key(value) < key(p.data)) {
      p.left = insert(p.left, value, key)
    } else {
      p.right = insert(p.right, value, key)
    }
    balance(p)
  }

}
