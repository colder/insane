package insane
package predefined

import annotations._

@AbstractsClass("java.net.URLConnection")
abstract class javanetURLConnection {
  @AbstractsMethod("java.net.URLConnection.getContentEncoding(()java.lang.String)")
  def __getContentEncoding(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.net.URLConnection.getContentType(()java.lang.String)")
  def __getContentType(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.net.URLConnection.getInputStream(()java.io.InputStream)")
  def __getInputStream(): java.io.InputStream
}
