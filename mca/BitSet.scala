package mca

import java.util.{BitSet => JavaBitSet}

class BitSet(bs: JavaBitSet) {

  private[BitSet] def bitSet = bs

  def |(that: BitSet) = performOperation(_.or(that.bitSet))

  def &(that: BitSet) = performOperation(_.and(that.bitSet))

  def &~(that: BitSet) = performOperation(_.andNot(that.bitSet))

  def +(v: Int) = performOperation(_.set(v))

  def -(v: Int) = performOperation(_.clear(v))

  def isEmpty = bs.isEmpty

  def next(v: Int): Option[Int] = {
    val nextSet = bs.nextSetBit(v)
    if (nextSet > -1)
      Some(nextSet)
    else
      None
  }

  def size = bs.cardinality

  private def performOperation(op: JavaBitSet => Unit): BitSet = {
    val newBitSet = bs.clone().asInstanceOf[JavaBitSet]
    op(newBitSet)
    BitSet(newBitSet)
  }

  override def toString = bs.toString

}

object BitSet {
  def apply(bs: JavaBitSet) = new BitSet(bs)

  def apply(size: Int) = new BitSet(new JavaBitSet(size))

  def filled(size: Int) = {
    val bs = new JavaBitSet(size)
    bs.set(0, size)
    BitSet(bs)
  }

  implicit def javaBitSetToBitSet(bs: JavaBitSet) = new BitSet(bs)
}
