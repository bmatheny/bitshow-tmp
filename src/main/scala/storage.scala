package bitshow

import org.sedis._
import redis.clients.jedis._
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.binary.Base64

case class Item(contentType: String, bytes: Array[Byte])

trait Storage {
  def get(id: String): Option[Item]
  def put(item: Item): String
  def list(): List[Item]
}

object DefaultStore extends VectorStore

trait VectorStore extends Storage { self =>
  private var vector = Vector.empty[Item]
  def get(id: String) = self.synchronized {
    import scala.util.control.Exception._
    allCatch.opt { vector(id.toInt) }
  }
  def put(bytes: Item) =
    self.synchronized {
      vector = vector :+ bytes
      (vector.length - 1).toString
    }
  def list(): List[Item] = vector.toList
}

class RedisStorage extends Storage {
  
  val pool = new Pool(new JedisPool(new JedisPoolConfig(), "localhost", 6379, 2000));

  override def get(id: String): Option[Item] = {
    val hash = "suck"
    val res = pool.withClient { client =>
       client.get(id) match {
         case Some(s) => Some(Item(hash, Base64.decodeBase64(s)))
         case _ => None
       }
    }
    return res
  }
  override def put(item: Item): String = {
    pool.withClient { client =>
        val hash = DigestUtils.sha256Hex(item.bytes)
        val filebytes = Base64.encodeBase64String(item.bytes)
        client.set(hash, filebytes)
        hash
    }
  }
  override def list(): List[Item] = Nil

}   
