package insane
package predefined

import annotations._

@AbstractsClass("java.lang.reflect.Method")
abstract class javalangreflectMethod {
  @AbstractsMethod("java.lang.reflect.Method.getDeclaringClass(()java.lang.Class)")
  def __getDeclaringClass(): java.lang.Class[_]
  @AbstractsMethod("java.lang.reflect.Method.getName(()java.lang.String)")
  def __getName(): java.lang.String = {
    ""
  }
  @AbstractsMethod("java.lang.reflect.Method.getParameterTypes(()Array[java.lang.Class])")
  def __getParameterTypes(): Array[java.lang.Class[_]] = {
    new Array[java.lang.Class[_]](1)
  }
  @AbstractsMethod("java.lang.reflect.Method.getReturnType(()java.lang.Class)")
  def __getReturnType(): java.lang.Class[_]
  @AbstractsMethod("java.lang.reflect.Method.invoke((x$1: java.lang.Object, x$2: Array[java.lang.Object])java.lang.Object)")
  def __invoke(x1: java.lang.Object, x2: Array[java.lang.Object]): java.lang.Object
}
