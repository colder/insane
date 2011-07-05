package insane
package utils

object Hash {
  def sha1(str: String): String = {
    import java.security.MessageDigest

    val md = MessageDigest.getInstance("SHA-1")
    md.update(str.getBytes("UTF-8"), 0, str.length)
    val bytes = md.digest()

    toHex(bytes)
  }

  private def toHex(bytes: Array[Byte]): String = {
    bytes.map{ b => String.format("%02X", new java.lang.Integer(b & 0xff)) }.mkString
  }
}
