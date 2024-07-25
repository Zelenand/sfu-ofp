case class MapNode(jdata: Map[String, Any]) extends Serializable {
  def get(key: String): Any = {
    jdata.getOrElse(key, throw new NoSuchElementException(s"No key = $key"))
  }

  def contains(key: String): Boolean = {
    jdata.contains(key)
  }

  override def toString: String = jdata.toString()
}
