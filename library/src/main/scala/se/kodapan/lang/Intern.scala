package se.kodapan.lang

import java.io.Serializable


/**
  * @author kalle
  * @since 2013-05-04 17:37
  */
trait Intern[T] extends Serializable {
  def intern(obj: T): T
}

/**
  * @author kalle
  * @since 2013-05-04 17:38
  */
@SerialVersionUID(1l)
class InternImpl[T] extends Intern[T] with Serializable {
  private val map = new java.util.HashMap[T, T]()

  override def intern(obj: T) = {
    var interned = map.get(obj)
    if (interned == null) {
      map.put(obj, obj)
      interned = obj
    }
    interned
  }

  override def toString = "InternImpl{" + "map.size=" + map.size + '}'
}
