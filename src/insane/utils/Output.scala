package insane
package utils

import java.io._
import java.net.{InetAddress, Socket, SocketException}

object OutputHandlers {

  trait Output {
    def println(str: String): Unit
    def print(str: String): Unit
  }

  class Console extends Output {
    def println(str: String): Unit = System.out.println(str)
    def print(str: String): Unit = System.out.print(str)
  }

  class Debug extends Output {
    val port = 3333;

    var out: PrintWriter = null

    def open = {
      try {
        val ia = InetAddress.getByName("localhost")
        val socket = new Socket(ia, port)
        out = new PrintWriter(socket.getOutputStream)
        print("\033[2J\033[1;1H");
        println("Insane connected!")
      } catch {
        case e =>
      }
    }

    def println(str: String): Unit = if (out != null) {
      out.write(str+"\n")
      out.flush
    }

    def print(str: String): Unit = if (out != null) {
      out.write(str)
      out.flush
    }

    open
  }
}
