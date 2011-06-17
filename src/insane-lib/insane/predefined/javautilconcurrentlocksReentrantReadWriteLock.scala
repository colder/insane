package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.locks.ReentrantReadWriteLock")
class javautilconcurrentlocksReentrantReadWriteLock {
  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.<init>(()java.util.concurrent.locks.ReentrantReadWriteLock)")
  def PLOPINIT() : java.util.concurrent.locks.ReentrantReadWriteLock = { new java.util.concurrent.locks.ReentrantReadWriteLock() }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.readLock(()java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock)")
  def readLock() : java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock = { new java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock() }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock.lock(()Unit)")
  def lock() : Unit = { () }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock.unlock(()Unit)")
  def unlock() : Unit = { () }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.writeLock(()java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock)")
  def writeLock() : java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock = { new java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock() }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock.lock(()Unit)")
  def lock() : Unit = { () }

  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock.unlock(()Unit)")
  def unlock() : Unit = { () }

}
