package insane
package predefined

import annotations._

@AbstractsClass("java.net.URL")
class javanetURL {
  @AbstractsMethod("java.net.URLConnection.getContentEncoding(()java.lang.String)")
  def getContentEncoding() : java.lang.String = { "" }

  @AbstractsMethod("java.net.URLConnection.getContentType(()java.lang.String)")
  def getContentType() : java.lang.String = { "" }

  @AbstractsMethod("java.net.URLConnection.getInputStream(()java.io.InputStream)")
  def getInputStream() : java.io.InputStream = { new java.io.InputStream() }

  @AbstractsMethod("java.net.URL.<init>((x$1:java.lang.String)java.net.URL)")
  def PLOPINIT(x1 : java.lang.String) : java.net.URL = { new java.net.URL() }

  @AbstractsMethod("java.net.URL.<init>((x$1:java.net.URL, x$2:java.lang.String)java.net.URL)")
  def PLOPINIT(x1 : java.net.URL, x2 : java.lang.String) : java.net.URL = { new java.net.URL() }

  @AbstractsMethod("java.net.URL.openConnection(()java.net.URLConnection)")
  def openConnection() : java.net.URLConnection = { new java.net.URLConnection() }

  @AbstractsMethod("java.net.URL.openStream(()java.io.InputStream)")
  def openStream() : java.io.InputStream = { new java.io.InputStream() }

  @AbstractsMethod("java.net.URL.toExternalForm(()java.lang.String)")
  def toExternalForm() : java.lang.String = { "" }

  @AbstractsMethod("java.net.URL.toString(()java.lang.String)")
  def toString() : java.lang.String = { "" }

}
