package insane
package utils

abstract class ProgressBar(var max: Int, val size: Int = 20) {
  protected var current    = 0
  protected var drawn      = 0
  protected var lastStr    = ""
  protected var maxlength  = 0

  def draw() {
    if (drawn > 0) {
      clear()
    }
    display()
    drawn += 1
  }

  def progress = size*current/max
  def percents = 100*current/max


  def clear()

  def end()

  def display()

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
}
class PlainProgressBar(_max: Int, _size: Int = 40) extends ProgressBar(_max, _size) {
  def clear() {
  }
  def end() {
  }

  def display() {
    println("Progress: "+current+"/"+max+" ("+percents+"%)")
  }
}

class HiddenProgressBar(_max: Int, _size: Int = 40) extends PlainProgressBar(_max, _size) {
  override def display() {
  }
}

class ConsoleProgressBar(_max: Int, blockSize: Int = 40) extends ProgressBar(_max, blockSize*8) {
  private var block      = "█"
  private var bars       = List(" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉")
  private var indicators = List("-", "\\", "|", "/")

  def clear() {
    print("\b"*lastStr.length)
  }

  def end() {
    println
  }

  def display() {
    val offset   = drawn%indicators.size

    val fullBlocks = progress/8;
    val lastBlock  = progress%8;

    var str = Console.MAGENTA+"info"+Console.RESET+": ┃"+block*fullBlocks+(if (fullBlocks < blockSize) bars(lastBlock) else "")+(" "*(blockSize-fullBlocks-1))+Console.RESET+"┃ "+percents+"% "+(if (percents < 100) indicators(offset) else "")

    if (str.length < maxlength) {
      str += " "*(maxlength-str.length)
    } else {
      maxlength = str.length
    }

    print(str)
    lastStr = str
  }
}
