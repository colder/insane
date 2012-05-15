package insane
package predefined

import annotations._

@AbstractsClass("java.net.URL")
abstract class javanetURL {
  @AbstractsMethod("java.net.URL.<init>((x$1: java.lang.String)java.net.URL)")
  def ____init__(x1: java.lang.String): javanetURL = {
    this
  }
  @AbstractsMethod("java.net.URL.<init>((x$1: java.net.URL, x$2: java.lang.String)java.net.URL)")
  def ____init__(x1: java.net.URL, x2: java.lang.String): javanetURL = {
    this
  }
  @AbstractsMethod("java.net.URL.openConnection(()java.net.URLConnection)")
  def __openConnection(): java.net.URLConnection
  @AbstractsMethod("java.net.URL.openStream(()java.io.InputStream)")
  def __openStream(): java.io.InputStream
  @AbstractsMethod("java.net.URL.toExternalForm(()java.lang.String)")
  def __toExternalForm(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.net.URL.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
}
