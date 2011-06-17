package insane
package predefined

import annotations._

@AbstractsClass("java.lang.reflect.Method")
class javalangreflectMethod {
  @AbstractsMethod("java.lang.reflect.Method.getDeclaringClass(()java.lang.Class)")
  def getDeclaringClass() : java.lang.Class = { new java.lang.Class() }

  @AbstractsMethod("java.lang.reflect.Method.getName(()java.lang.String)")
  def getName() : java.lang.String = { "" }

  @AbstractsMethod("java.lang.reflect.Method.getParameterTypes(()Array[java.lang.Class])")
  def getParameterTypes() : Array[java.lang.Class] = { new Array[java.lang.Class]() }

  @AbstractsMethod("java.lang.reflect.Method.getReturnType(()java.lang.Class)")
  def getReturnType() : java.lang.Class = { new java.lang.Class() }

  @AbstractsMethod("java.lang.reflect.Method.invoke((x$1:java.lang.Object, x$2:Array[java.lang.Object])java.lang.Object)")
  def invoke(x1 : java.lang.Object, x2 : Array[java.lang.Object]) : java.lang.Object = { new java.lang.Object() }

}
