package insane
package predefined

import annotations._

@AbstractsClass("java.net.URLConnection")
class javanetURLConnection {
  @AbstractsMethod("java.net.URLConnection.getContentEncoding(()java.lang.String)")
  def getContentEncoding(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.net.URLConnection.getContentType(()java.lang.String)")
  def getContentType(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.net.URLConnection.getInputStream(()java.io.InputStream)")
  def getInputStream(): java.io.InputStream = {
    new java.io.InputStream()
  }
}
