package insane
package predefined

import annotations._

@AbstractsModuleClass("java.lang.Integer")
class javaLangInteger {
    @AbstractsStaticMethod("java.lang.Integer.valueOf((x$1: Int)Integer)")
    def valueOf(i: Int): java.lang.Integer = {
      return new java.lang.Integer(i);
    }
}
