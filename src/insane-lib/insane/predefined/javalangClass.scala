package insane
package predefined

import annotations._

@AbstractsClass("java.lang.Class")
abstract class javalangClass {
  @AbstractsMethod("java.lang.Class.forName((x$1: java.lang.String)java.lang.Class)")
  def __forName(x1: java.lang.String): java.lang.Class[_]
  @AbstractsMethod("java.lang.Class.getComponentType(()java.lang.Class)")
  def __getComponentType(): java.lang.Class[_]
  @AbstractsMethod("java.lang.Class.getField((x$1: java.lang.String)java.lang.reflect.Field)")
  def __getField(x1: java.lang.String): java.lang.reflect.Field
  @AbstractsMethod("java.lang.Class.getInterfaces(()Array[java.lang.Class])")
  def __getInterfaces(): Array[java.lang.Class[_]] = {
    new Array[java.lang.Class[_]](1)
  }
  @AbstractsMethod("java.lang.Class.getMethods(()Array[java.lang.reflect.Method])")
  def __getMethods(): Array[java.lang.reflect.Method] = {
    new Array[java.lang.reflect.Method](1)
  }
  @AbstractsMethod("java.lang.Class.getMethod((x$1: java.lang.String, x$2: Array[java.lang.Class])java.lang.reflect.Method)")
  def __getMethod(x1: java.lang.String, x2: Array[java.lang.Class[_]]): java.lang.reflect.Method
  @AbstractsMethod("java.lang.Class.getName(()java.lang.String)")
  def __getName(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Class.getPackage(()java.lang.Package)")
  def __getPackage(): java.lang.Package
  @AbstractsMethod("java.lang.Class.getResourceAsStream((x$1: java.lang.String)java.io.InputStream)")
  def __getResourceAsStream(x1: java.lang.String): java.io.InputStream
  @AbstractsMethod("java.lang.Class.getSimpleName(()java.lang.String)")
  def __getSimpleName(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.Class.getSuperclass(()java.lang.Class)")
  def __getSuperclass(): java.lang.Class[_]
  @AbstractsMethod("java.lang.Class.isArray(()Boolean)")
  def __isArray(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Class.isAssignableFrom((x$1: java.lang.Class)Boolean)")
  def __isAssignableFrom(x1: java.lang.Class[_]): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Class.isPrimitive(()Boolean)")
  def __isPrimitive(): Boolean = {
    true
  }
  @AbstractsMethod("java.lang.Class.newInstance(()java.lang.Object)")
  def __newInstance(): java.lang.Object
  @AbstractsMethod("java.lang.Class.toString(()java.lang.String)")
  def __toString(): java.lang.String = {
    ""
  }
}
