package insane
package predefined

import annotations._

@AbstractsClass("org.xml.sax.InputSource")
class orgxmlsaxInputSource {
  @AbstractsMethod("org.xml.sax.InputSource.<init>((x$1: java.io.InputStream)org.xml.sax.InputSource)")
  def __init__(x1: java.io.InputStream): org.xml.sax.InputSource = {
    new org.xml.sax.InputSource()
  }
  @AbstractsMethod("org.xml.sax.InputSource.<init>((x$1: java.io.Reader)org.xml.sax.InputSource)")
  def __init__(x1: java.io.Reader): org.xml.sax.InputSource = {
    new org.xml.sax.InputSource()
  }
  @AbstractsMethod("org.xml.sax.InputSource.<init>((x$1: java.lang.String)org.xml.sax.InputSource)")
  def __init__(x1: java.lang.String): org.xml.sax.InputSource = {
    new org.xml.sax.InputSource()
  }
}
