// src/main/scala/epidemic/ReplayBuffer.scala
package epidemic

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import epidemic.State._ 

final case class Transition(s: State, a: Int, r: Double, s2: State, done: Boolean)

final class ReplayBuffer(capacity: Int):
  private val buf = new ArrayBuffer[Transition](capacity)
  private var idx = 0
  private val rnd = new Random(123)

  def push(tr: Transition): Unit =
    if buf.size < capacity then buf += tr
    else { buf(idx) = tr; idx = (idx + 1) % capacity }

  def size: Int = buf.size

  def sample(n: Int): Array[Transition] =
    rnd.shuffle(buf).take(n).toArray
