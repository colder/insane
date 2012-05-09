package insane
package utils

abstract class ProgressBar(var max: Int, val size: Int = 20) {
  protected var current    = 0
  protected var drawn      = 0
  protected var lastStr    = ""
  protected var maxlength  = 0
  protected var postfix    = ""

  def draw() {
    if (!ended) {
      val str = getStr

      if (str != lastStr) {
        if (drawn > 0) {
          clear()
        }
        print(str)
        drawn += 1
        lastStr = str
      }
    }
  }

  def setPostfix(str: String) {
    postfix = str
  }


  def progress = size*current/max
  def percents = 100*current/max


  def clear()

  var ended = false
  def end() = {
    ended = true
  }

  def getStr: String

  def tick() = ticks(1)
  def ticks(amount: Int) {
    current += amount;
    if (current > max) {
      current = max
    }
  }

  def setMax(newMax: Int) {
    max = newMax
  }

  def setCurrent(current: Int) {
    this.current = current
  }
}
class PlainProgressBar(_max: Int, _size: Int = 40) extends ProgressBar(_max, _size) {
  def clear() {
  }

  def getStr = {
    if (max < 50 || current % (max/50) == 0) {
      "info   : Progress: "+current+"/"+max+" ("+percents+"%) "+postfix+"\n"
    } else {
      lastStr
    }
  }
}

class HiddenProgressBar(_max: Int, _size: Int = 40) extends PlainProgressBar(_max, _size) {
  override def getStr = ""
}

class ConsoleProgressBar(_max: Int, blockSize: Int = 40) extends ProgressBar(_max, blockSize*8) {
  private var block      = "█"
  private var bars       = List(" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉")
  private var indicators = List("-", "\\", "|", "/")

  def clear() {
    print("\b"*lastStr.length)
  }

  override def end() {
    super.end()
    println
  }

  def getStr = {
    val offset   = drawn%indicators.size

    val fullBlocks = progress/8;
    val lastBlock  = progress%8;

    var str = Console.MAGENTA+"info"+Console.RESET+"   : ┃"+block*fullBlocks+(if (fullBlocks < blockSize) bars(lastBlock) else "")+(" "*(blockSize-fullBlocks-1))+Console.RESET+"┃ "+percents+"% "+(if (percents < 100) indicators(offset) else "  ")+postfix

    if (str.length < maxlength) {
      str += " "*(maxlength-str.length)
    } else {
      maxlength = str.length
    }

    str
  }
}
