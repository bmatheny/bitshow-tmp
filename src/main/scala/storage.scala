package bitshow

import org.sedis._
import redis.clients.jedis._
import org.apache.commons.codec.digest.DigestUtils._
import org.apache.commons.codec.binary.Base64._

case class Item(contentType: String, bytes: Array[Byte])

trait Storage {
  def get(id: String): Option[Item]
  def put(item: Item): String
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
} 

class RedisStorage extends Storage {
  
  val pool = new Pool(new JedisPool(new JedisPoolConfig(), "localhost", 6379, 2000));
  val baser = new Base64()

  override def get(id: String): Option[Item] = {
    val res = pool.withClient { client =>
       client.get(id) match {
          s: String => Some(Item(hash, baser.decodeBase64String(filebytes)))
          _ => None
       }
    }
    return res
  }
  override def put(item: Item): String = {
    val hash = pool.withClient { client =>
        val hash = DigestUtils.sha256Hex(item.bytes)
        val filebytes = baser.encodeBase64String(item.bytes)
        client.set(hash, filebytes)
        hash
    }
    hash
  }
}   
