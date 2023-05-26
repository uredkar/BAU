trait HeapInterface:

  /** @return an empty binomial heap */
  def empty: List[Node]
  /** @return whether the given `heap` is empty or not */
  def isEmpty(heap: List[Node]): Boolean

  /** Insert the given `value` in the `heap` */
  def insert(value: Int, heap: List[Node]): List[Node]
  /** Merge two heaps */
  def meld(heap1: List[Node], heap2: List[Node]): List[Node]

  /** Find the minimum value of a heap, or raise an error if the heap is empty */
  def findMin(heap: List[Node]): Int
  /** Delete the minimum value of a heap, or raise an error if the heap is empty */
  def deleteMin(heap: List[Node]): List[Node]

  case class Node(value: Int, rank: Int, children: List[Node])

end HeapInterface

class BinomialHeap extends HeapInterface:

  protected def root(node: Node) = node.value
  protected def rank(node: Node) = node.rank
  protected def link(node1: Node, node2: Node): Node = // node1.rank == node2.rank
    if node1.value <= node2.value then
      Node(node1.value, node1.rank + 1, node2 :: node1.children)
    else
      Node(node2.value, node2.rank + 1, node1 :: node2.children)
  protected def ins(node: Node, heap: List[Node]): List[Node] = heap match
    case Nil => List(node)
    case node2 :: heap2 => // node.rank <= node2.rank
      if node.rank < node2.rank then node :: node2 :: heap2 else ins(link(node, node2), heap2)

  def empty: List[Node] = Nil

  def isEmpty(heap: List[Node]): Boolean = heap.isEmpty

  def insert(value: Int, heap: List[Node]): List[Node] =
    ins(Node(value, 0, Nil), heap)

  def meld(heap1: List[Node], heap2: List[Node]): List[Node] = (heap1, heap2) match
    case (Nil, heap) => heap
    case (heap, Nil) => heap
    case (node1 :: heap1, node2 :: heap2) =>
      if node1.rank < node2.rank then node1 :: meld(heap1, node2 :: heap2)
      else if node2.rank < node1.rank then node2 :: meld(node1 :: heap1, heap2)
      else ins(link(node1, node2), meld(heap1, heap2))

  def findMin(heap: List[Node]): Int = heap match
    case Nil => throw new NoSuchElementException("min of empty heap")
    case node :: Nil => root(node)
    case node :: heap =>
      val x = findMin(heap)
      if root(node) <= x then root(node) else x

  def deleteMin(heap: List[Node]): List[Node] = heap match
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case node :: heap =>
      def getMin(node: Node, heap: List[Node]): (Node, List[Node]) = heap match
        case Nil => (node, Nil)
        case node2 :: heap2 =>
          val (node3, heap3) = getMin(node2, heap2)
          if root(node) <= root(node3) then (node, heap) else (node3, node :: heap3)
      val (Node(_, _, children), heap3) = getMin(node, heap)
      meld(children.reverse, heap3)

end BinomialHeap

val bh = new BinomialHeap
val heap1 = bh.insert(1, bh.insert(2, bh.empty))
val heap2 = bh.insert(2, bh.insert(5, bh.empty))
val meld = bh.meld(heap1,heap2)
meld

bh.findMin(heap2)
bh.findMin(heap1)
bh.findMin(meld)
bh.deleteMin(meld)
bh.deleteMin(heap1)

val s = "8379" 
s.splitAt(1)
s.splitAt(2)
def split(digit: String) : Seq[Seq[String]] = 
    if digit.isEmpty then Seq(Nil)
    else
        for 
            splitPoint <- 1 to digit.length
            (first,remain) = digit.splitAt(splitPoint)
            digitSeq <- split(remain)
        yield 
            println(s"split point $splitPoint first $first, remain $remain")
            first +: digitSeq
val ss = split(s)                    
ss.map(elm => { 
    println(elm)
    //elm.map(println _)
})


class Mnemonics(dictionary: Set[String]):

  private val keys: Map[Char, String] = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )

  private val letterToDigit: Map[Char, Char] =
    for
      (digit, letters) <- keys
      letter           <- letters
    yield letter -> digit

  private def wordToDigits(word: String): String =
    word.toUpperCase.map(letterToDigit)

  private val index: Map[String, Set[String]] =
    dictionary.groupBy(wordToDigits)

  def ofPhoneNumber(digits: String): Seq[Seq[String]] =
    if digits.isEmpty then Seq(Nil)
    else
      for
        splitPoint <- 1 to digits.length
        (firstDigits, remainingDigits) = digits.splitAt(splitPoint)
        word  <- index.getOrElse(firstDigits, Nil)
        words <- ofPhoneNumber(remainingDigits)
      yield word +: words

end Mnemonics

val dictionary = Set(
  "Scala",
  "rocks",
  "is",
  "fun",
  "love",
  "thank",
  "me",
  "you",
  "of"
)

val mnemonics = Mnemonics(dictionary)

mnemonics.ofPhoneNumber("7225276257")
// res6: Seq[Seq[String]] = Vector(List(Scala, rocks))
mnemonics.ofPhoneNumber("7225247386")
// res7: Seq[Seq[String]] = Vector(List(Scala, is, fun))
mnemonics.ofPhoneNumber("7225284265968")
// res8: Seq[Seq[String]] = Vector(List(Scala, thank, you))
mnemonics.ofPhoneNumber("968568363")
// res9: Vector(List(you, love, me), List(you, love, of))