package mca

trait BronKerbosch {

  def size: Int

  def neighbours(v: Int): BitSet

  def selectPivot(bk: BronKerbosch, P: BitSet, X: BitSet): Int = P next(0) get

  def maximalCliques = extend(BitSet(size), BitSet.filled(size), BitSet(size))

  private def extend(R: BitSet, P: BitSet, X: BitSet): List[BitSet] = {
    @annotation.tailrec
    def processCandidate(lastCandidate: Int, candidates: BitSet, nextP: BitSet, nextX: BitSet, cliques: List[BitSet]): List[BitSet]= {
      candidates.next(lastCandidate + 1) match {
        case Some(c) => {
          val newCliques = cliques ::: extend(R + c, nextP & neighbours(c), nextX & neighbours(c))
          processCandidate(c, candidates, nextP - c, nextX + c, newCliques)
        }
        case None => cliques
      }
    }
    if (!P.isEmpty)
      processCandidate(-1, P &~ neighbours(selectPivot(this, P, X)), P, X, Nil)
    else if (X.isEmpty)
      List(R)
    else
      Nil
  }
}

trait TomitaPivotSelection {

  def selectPivot(bk: BronKerbosch, P: BitSet, X: BitSet): Int = {
    val union = P | X
    var max = -1

    @annotation.tailrec
    def loop(bit: Int, pivot: Int): Int = {
      union.next(bit + 1) match {
        case Some(b) => {
          val setSize = (P & bk.neighbours(b)).size
          if (setSize > max) {
            max = setSize
            loop(b, b)
          }
          else
            loop(b, pivot)
        }
        case None => pivot
      }
    }
    loop(-1, -1)
  }
}
