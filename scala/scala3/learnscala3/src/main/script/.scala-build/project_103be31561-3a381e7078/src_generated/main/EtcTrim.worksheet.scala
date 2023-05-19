



object `EtcTrim.worksheet` {
def args = `EtcTrim.worksheet_sc`.args$
/*<script>*/import scala.collection.mutable.ArrayBuffer
import java.lang.ProcessBuilder.Redirect
import scala.util.Random
import scala.util.control.NonFatal
def trimLeft(str: String) = str.dropWhile(_.isWhitespace)
def trimRight(str: String) = str.take(str.lastIndexWhere(!_.isWhitespace)+1)
def trimRight2(str: String) = trimLeft(str.reverse).reverse
def trim(str: String) = str.trim()

trimLeft(">hello>  <wo<  <rld < <")
trimRight("  <<s s s 1 xxx")
trimRight2("  <<s s s 1 xxx")
trim("  >red car<  ")

def testTrim() = {
  val str = "  \u001F  String with spaces \t  \n  \r "
  println("original  : |" + str + "|")
  println("trimLeft  : |" + trimLeft(str) + "|")
  println("trimRight : |" + trimRight(str) + "|")
  println("trimRight2: |" + trimRight2(str) + "|")
  println("trim      : |" + trim(str) + "|")
}

testTrim()
println(1+2*3)
if true then println("true")
def showPrice(x:Double, p : Double) : String = 
    val price = x * p
    if price > 0 then
        "this is too expensive"
    else
        price.toString()

val a = 1
def square(x:Int): Int = 
    val y = 
        val z = 10 * 3
        z
    println(s"$x,$y,$a")    
    y
square(10)    

val tenSq1 = 10 * 10
def tenSq2 = 10 * 10
tenSq2
tenSq2


case class Note():
  def tone: Int = ??? 
  def duration: Int = ???

sealed trait Shape
case class Rectangle(width: Int, height:Int) extends Shape
case class Circle(radius:Int) extends Shape

val rect = Rectangle(10,20)
val cir = Circle(10)

def m(s:Shape) = s match
    case c: Circle => println(s"radius ${c.radius}")
    case Rectangle(w,h) => println(s"rect $w $h")

    //case Circle(r) => println(s"radius $r")
    
val someShape1: Shape = rect
val someShape2: Shape = cir
m(rect)
m(cir)

enum PC(val v:String):
    case Red extends PC("X")
    case Blue extends PC("Y")

val col = PC.Red
val col2: PC = PC.Blue
PC.valueOf("Red")
PC.values(0)
col.v

case class Contact (name: String,email: String)


val endWith: Contact => Boolean = 
    //contact => contact.email.endsWith(".org")
    //inp => inp.email.endsWith(".org")
    _.email.endsWith(".org") 

val contact1 = Contact("test","test.org")
val contact2 = Contact("test","test.or2g")
endWith(contact1)
endWith(contact2)

val incr1 = (x: Int) => x + 1
val incr2 = (_: Int) + 1
incr1(10)
incr2(10)

val placeholder = (_: Int) + 1

// does not use its argument 
val wildcard = (_: Int) => 42

import scala.collection.mutable


val xs1 = List("1","2","3")
val xs2 = List("4","5","6")
val ab = mutable.ArrayBuffer.empty[String]
ab.clear()
println(s"array buffer ab $ab")

ab ++= List("a","b","c")
ab
val mp = Map.empty[String,Boolean] 

ab ++= xs1
ab ++= xs2
ab
println(s"array buffer ab $ab")
for {
    x <- xs1 
    y <- xs2
} {      
    ab += x
    ab += y
} 

ab
println(s"for loop is not doing what you think change your thinking for scala array buffer ab $ab")

mutable.ArrayBuffer.empty[Int]
List.empty[Int]
//new List[Int]

Map.empty[String,Int] + ("a" -> 1) + ("b" -> 2)
val m1 = Map("a"->1) + ("b" -> 2)
val m2 = Map("a" -> 1,"b" -> 2)
m1.contains("a1")
m1.get("a")
val v = m1.get("a") match 
                case Some(v) => v
                case None => -1 
v
val maybeInt : Option[Int] = Some(1)
maybeInt.map(x => x + 1).getOrElse(-1)



m1.get("a").get
m1.get("a").getOrElse("Found")
m1.get("xx")
m1.get("xx").getOrElse("Not Found")  //this should fail as xx is not found in the m1 and hence it returns None

val vnf = m1.get("xx") match 
                case Some(v) => v
                case None => -1 
vnf // not found

val isthere = ab.contains("4")
ab.filter(x => x == "4")
ab.foldLeft("x")((acc,v) => acc + v)
val first_value = ab.find(x => x == "4")

val pastry = List("croissant", "cake", "pain au chocolat")
val ingredients = (s: String) =>
  if(s == "croissant") List("flour", "butter")
  else if(s == "cake") List("flour", "eggs", "sugar", "butter")
  else List()

pastry.map(ingredients)
val seq1 = pastry.flatMap(ingredients)
seq1.head
seq1.tail
seq1.last
ab
ab(10)



val data = List("Alice" -> 42,"Bob" -> 30,"Werner" -> 77,"Owl" -> 6)
data.sortBy((_,_))
data.sortBy((_,age) => age)
data.sortBy((name, _) => name)

def factorial(n: Int): Int = 
    if n == 0 then 1
    else 
        n * factorial(n-1) // last is n * fn

def factorial_tail(i:Int): Int  = {
    def tail_fact(x: Int, acc: Int): Int = {
        if x == 0 then 
            acc
        else 
            tail_fact(x-1,acc * x)
    }
    tail_fact(i,1) // last call
}
factorial(15)
factorial_tail(15) 

val map: Map[String, Seq[String]] = Map(
  "key1" -> Seq("value1", "value2", "value3"),
  "key2" -> Seq("value2", "value3", "value4"),
  "key3" -> Seq("value3", "value4", "value5")
)

val commonValues: Set[String] = map.values.reduce(_.diff(_)).toSet

val filteredMap: Map[String, Seq[String]] = map.view.mapValues(_.filter(commonValues)).toMap


println(filteredMap)
//val diffMap: Map[String, Int] = map1.diff(map2)


enum Grade:
  case Bad, Mediocre, Inadequate, Passable, Good, VeryGood, Excellent
case class Ballot(grades: Map[Candidate, Grade])
case class Candidate(name: String)
def median(grades: Seq[Grade]): Grade =
    if grades.size == 1 then 
        grades(0)
    else

        val sorted = grades.sortBy(grade => grade.ordinal)
        val middle = (grades.size + 1) / 2
        val median = if (grades.size + 1) % 2  == 1 then middle +  1 else middle
        sorted.apply(median-1)

median(Seq(Grade.Excellent))
median(Seq(Grade.Bad,Grade.Mediocre))
median(Seq(Grade.Passable,Grade.Bad,Grade.Mediocre,Grade.Excellent))
median(Seq(Grade.Bad,Grade.Mediocre,Grade.Passable,Grade.Good,Grade.Excellent))
median(Seq(Grade.Mediocre,Grade.Passable,Grade.Bad,Grade.Good,Grade.Excellent))
val grades1 = Seq(Grade.Bad,Grade.Mediocre)
grades1.sortBy(grade => grade.ordinal)

val grades2 = Seq(Grade.Mediocre,Grade.Bad)
grades2.sortBy(grade => grade.ordinal)

val grades3 = Seq(Grade.Mediocre)
val g3 = grades3.sortBy(grade => grade.ordinal)
g3.size
g3.size/2
g3.size%2
median(g3)

for (i <- 0 to 0) {
    val middle = (i+1) / 2
    val median = if (i+1) % 2  == 1 then middle +  1 else middle
    println(s"i = $i, middle = $middle, median = $median")
}

val tiramisu    = Candidate("Tiramisu")
val cremeBrulee = Candidate("Crème brûlée")
val cheesecake  = Candidate("Cheesecake")

//case class Election(description: String, candidates: Set[Candidate]):
//val election = Election("Best dessert", Set(tiramisu, cremeBrulee, cheesecake))

val ballot1 = Ballot(
    Map(
    tiramisu    -> Grade.Excellent,
    cremeBrulee -> Grade.Good,
    cheesecake  -> Grade.Inadequate
    )
)

val ballot2 = Ballot(
    Map(
    tiramisu    -> Grade.Excellent,
    cremeBrulee -> Grade.Passable,
    cheesecake  -> Grade.Good
    )
)

val ballot3 = Ballot(
    Map(
    tiramisu    -> Grade.VeryGood,
    cremeBrulee -> Grade.Inadequate,
    cheesecake  -> Grade.Good
    )
)

val ballots1 = Seq(ballot1, ballot2, ballot3)


val karting  = Candidate("Karting")
val sailing  = Candidate("Sailing")
val hiking   = Candidate("Hiking")
val cooking  = Candidate("Cooking")

val ballotAlice = Ballot(
    Map(
    karting -> Grade.Good,
    sailing -> Grade.Excellent,
    hiking  -> Grade.VeryGood,
    cooking -> Grade.VeryGood
    )
)

val ballotBob = Ballot(
    Map(
    karting -> Grade.VeryGood,
    sailing -> Grade.VeryGood,
    hiking  -> Grade.VeryGood,
    cooking -> Grade.Excellent
    )
)

val ballotCarol = Ballot(
    Map(
    karting -> Grade.Passable,
    sailing -> Grade.Mediocre,
    hiking  -> Grade.Good,
    cooking -> Grade.Good
    )
)
val ballots = Seq(ballotAlice, ballotBob, ballotCarol)
val allGrades = ballots.flatMap(b => b.grades)
println(allGrades)
val gradesPerCandidate: Map[Candidate, Seq[Grade]] =
    allGrades.groupMap(k => k._1)(v => v._2)
println(gradesPerCandidate.values)
gradesPerCandidate.values
    .filter(grade => grade.isEmpty == false)
    .map(g => median(g))
val bestMedianGrade: Grade = gradesPerCandidate.values
                                .filter(grade => grade.isEmpty == false)
                                .map(g => median(g))
                                .maxBy(g => g.ordinal)

val bestCandidates: Map[Candidate, Seq[Grade]] =
        gradesPerCandidate.map((k,v) => (k,v.filter(x => x.ordinal == bestMedianGrade.ordinal)))
        .filter((k,v) => v.isEmpty == false)
println("Best candidates")
println(bestCandidates.size)
println(bestCandidates)

bestCandidates.head._1

//val commonValues: Set[String] = map.values.reduce(_.diff(_)).toSet

//val filteredMap: Map[String, Seq[String]] = map.view.mapValues(_.filter(commonValues)).toMap

val common1 = bestCandidates.values.reduce((s1,s2) => s1.intersect(s2))
val common = bestCandidates.values.reduce((s1,s2) => s1.intersect(s2)).toSet.head

val bestCandidatesMinusOneMedianGrade: Map[Candidate, Seq[Grade]] =
    bestCandidates.map((k,v) => (k,v.zipWithIndex.filterNot { 
        case (element, index) => element == common && v.indexOf(common) == index
    }.map(_._1))).filter((k,v) => v.nonEmpty)
        

          
//seq.zipWithIndex.filterNot { case (element, index) => element == common && seq.indexOf(element) == index }.map(_._1)

//println(bestCandidatesMinusOneMedianGrade)

val candidate1 = Candidate("Candidate 1")
val candidate2 = Candidate("Candidate 2")
val candidate3 = Candidate("Candidate 3")
val candidates = Set(candidate1, candidate2, candidate3)
//val election = Election("Some election", candidates)

def randomGrade(): Grade =
    Grade.values(Random.nextInt(Grade.values.length))

def randomBallot(): Ballot =
    Ballot(candidates.map(_ -> randomGrade()).toMap)

val voters = 50

for
    _       <- 1 to 10
    ballots  = Seq.fill(voters)(randomBallot())
    
    //winner   = election.elect(ballots)
    //loosers  = candidates.filter(_ != winner)
do
    //println(ballots)
    val allGrades: Seq[(Candidate, Grade)] = ballots.flatMap(b => b.grades)
    val gradesPerCandidate: Map[Candidate, Seq[Grade]] =
      allGrades.groupMap(k => k._1)(v => v._2)
    //if gradesPerCandidate.forall((candidate, grades) => grades.isEmpty) then
    val candidatesSeq = gradesPerCandidate.keys.toSeq
    val randomIndex   = util.Random.between(0, candidatesSeq.size)
    println(s"randomIndex $randomIndex")  
    val candidateMedians =
    candidates.map { candidate =>
        candidate -> median(ballots.map(_.grades(candidate)))
    }.toMap
    val highestMedian = candidateMedians.values.maxBy(_.ordinal)    /*</script>*/ /*<generated>*/
/*</generated>*/
}

object `EtcTrim.worksheet_sc` {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }
  def main(args: Array[String]): Unit = {
    args$set(args)
    `EtcTrim.worksheet`.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

