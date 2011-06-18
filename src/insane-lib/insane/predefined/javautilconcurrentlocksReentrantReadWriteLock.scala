package insane
package predefined

import annotations._

@AbstractsClass("java.util.concurrent.locks.ReentrantReadWriteLock")
class javautilconcurrentlocksReentrantReadWriteLock {
  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.<init>(()java.util.concurrent.locks.ReentrantReadWriteLock)")
  def __init__(): javautilconcurrentlocksReentrantReadWriteLock = {
    this
  }
  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.readLock(()java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock)")
  def readLock(): java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock = {
    new java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock()
  }
  @AbstractsMethod("java.util.concurrent.locks.ReentrantReadWriteLock.writeLock(()java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock)")
  def writeLock(): java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock = {
    new java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock()
  }
}
