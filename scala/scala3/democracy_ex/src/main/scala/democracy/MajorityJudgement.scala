package democracy

/**
 * A grade to assign to a candidate. There are seven possible grades (from
 * the worst to the best): `Bad`, `Mediocre`, `Inadequate`, `Passable`, `Good`,
 * `VeryGood`, and `Excellent`.
 *
 * Grades can be compared by using their `ordinal` method:
 *
 * {{{
 *   Grade.Mediocre.ordinal < Grade.Good.ordinal
 * }}}
 */
enum Grade:
  case Bad, Mediocre, Inadequate, Passable, Good, VeryGood, Excellent

object Grade:
  /**
   * @return The median grade of a collection of grades.
   *
   * The median grade can be computed by sorting the collection
   * and taking the element in the middle. If there are an even
   * number of grades, any of the two grades that are just before
   * or after the middle of the sequence are correct median values.
   *
   * Grades can be compared by using their `ordinal` method.
   *
   * Hints: use the following operations:
   * - `sortBy` and `ordinal` to sort the collection of grades,
   * - `size` to compute the number of elements,
   * - `apply` to select an element at a specific index.
   */
  def median(grades: Seq[Grade]): Grade =
    if grades.size == 1 then 
        grades(0)
    else
        val sorted = grades.sortBy(grade => grade.ordinal)
        val length = sorted.length
        var median = length / 2
        if (length % 2 == 0) {
          if (median - 1 >= 0)
            median = median - 1
          else
            median = median + 1            
        }
        sorted.apply(median)

end Grade

/**
 * A candidate in an election.
 * @param name (unique) name of the candidate (e.g., “Barack Obama”)
 */
case class Candidate(name: String)

/**
 * A ballot, which assigns a grade to each candidate of an election.
 * @param grades The grades assigned to each candidate
 */
case class Ballot(grades: Map[Candidate, Grade])

/**
 * An election is defined by a simple description and a set of possible
 * candidates.
 * @param description  Description of the election (e.g., “Presidential Election”)
 * @param candidates Possible candidates
 */
case class Election(description: String, candidates: Set[Candidate]):
  /**
   * @return The candidate that wins this election, according to the Majority
   *         Judgement voting process.
   *
   * @param ballots The ballots for this election
   *
   * The ballots ''must'' assign a grade to each of the `candidates` of this
   * election.
   */
  def elect(ballots: Seq[Ballot]): Candidate =
    assert(ballots.nonEmpty)
    assert(ballots.forall(_.grades.keySet == candidates))

    // Re-structure the data to get all the grades assigned to
    // each candidate by all the voters

    // First step: use the operation `flatMap` to flatten the ballots
    // into a single sequence containing the grades assigned to each
    // candidate by the voters.
    val allGrades: Seq[(Candidate, Grade)] = ballots.flatMap(b => b.grades)
      

    // Second step: use the operation `groupMap` to transform the
    // collection of pairs of `(Candidate, Grade)` into a `Map`
    // containing all the grades that were assigned to a given
    // `Candidate`.
    val gradesPerCandidate: Map[Candidate, Seq[Grade]] =
      allGrades.groupMap(k => k._1)(v => v._2)

    findWinner(gradesPerCandidate)
  end elect

  /**
   * @return The winner of this election, according to the Majority Judgement
   *         voting process.
   *
   * @param gradesPerCandidate The grades that have been assigned to each
   *                             candidate by the voters.
   */
  def findWinner(gradesPerCandidate: Map[Candidate, Seq[Grade]]): Candidate =
    // In case all the candidates have an empty collection of grades (this
    // can happen because of the tie-breaking algorithm, see below), the winner
    // is chosen by lottery from among the candidates.
    //println("findWinner")
    if gradesPerCandidate.forall((candidate, grades) => grades.isEmpty) then
      //println("TTTTTIIIIEEEE breaker")
      
      val candidatesSeq = gradesPerCandidate.keys.toSeq

      //println(s"candidates ${candidatesSeq.size} $candidatesSeq")
      val randomIndex   = util.Random.between(0, candidatesSeq.size)
      
      candidatesSeq(randomIndex)
    else
      // Otherwise, find the highest median grade assigned to a candidate.
      // Use the operation `values` to select the collections of grades,
      // then use the operation `filter` to keep only the non empty grades,
      // then use the operation `map` to compute the median value of each collection
      // of grades, and finally use the operation `maxBy` to find the highest
      // median grade.
      val bestMedianGrade: Grade = gradesPerCandidate.values
                                    .filter(grade => grade.isEmpty == false)
                                    .map(g => Grade.median(g))
                                    .maxBy(g => g.ordinal)

      // Use the operation `filter` to select all the candidates that got the
      // same best median grade (as the case may be)
      
      val bestCandidates: Map[Candidate, Seq[Grade]] =
        gradesPerCandidate.map((k,v) => (k,v.filter(x => x.ordinal >= bestMedianGrade.ordinal)))
        .filter((k,v) => v.isEmpty == false)
      // In case only one candidate got the best median grade, it’s the winner!
      if bestCandidates.size == 1 then
        // Use the operation `head` to retrieve the only element
        // of the collection `bestCandidates`
        bestCandidates.head._1
      else
        // Otherwise, there is a tie between several candidates. The tie-breaking
        // algorithm is the following:
        // “If more than one candidate has the same highest median-grade, the winner is
        // discovered by removing (one-by-one) any grades equal in value to the shared
        // median grade from each tied candidate's total. This is repeated until only one
        // of the previously tied candidates is currently found to have the highest
        // median-grade.” (source: https://en.wikipedia.org/wiki/Majority_judgment)
  
        // Use the operation `map` to transform each element of the `bestCandidates`.
        // And use the operation `diff` to remove one `bestMedianGrade` from the
        // grades assigned to the candidates.
        //val common1 = bestCandidates.values.reduce((s1,s2) => s1.intersect(s2))
        val common = bestCandidates.values.reduce((s1,s2) => s1.intersect(s2)).toSet
        //val common = common1.minBy(p => p.ordinal)
        
        //println(s"common grade $common")
        //println(s"$bestCandidates")
        val bestCandidatesMinusOneMedianGrade: Map[Candidate, Seq[Grade]] =
          bestCandidates.map((k,v) => (k,v.filterNot { 
            case element => v.isEmpty == false && common.contains(element)
          }.map(x => x)))
        //println(s"$bestCandidatesMinusOneMedianGrade")
        // Finally, call `findWinner` on the reduced collection of candidates,
        // `bestCandidatesMinusOneMedianGrade`.
        findWinner(bestCandidatesMinusOneMedianGrade)
  end findWinner

end Election
