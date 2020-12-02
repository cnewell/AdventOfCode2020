import scala.collection.mutable.HashSet
import scala.io.BufferedSource
import scala.io.Source

object Main {

  val TARGET = 2020

  /** Day 1 first challenge: find pair adding to 2020
   *
   *  O(n) Iterate once through the list once at the cost of memory
   *  to store the hashset of values we've already seen.
   *
   */
  def firstChallenge(f: BufferedSource): Unit = {
    val hash: HashSet[Int] = HashSet.empty[Int]

    // For each line...
    for (line <- f.getLines)
      try {
        val i = line.toInt

        // find the second value we need
        val j = TARGET - i

        if (hash(j)) {
          // if we've already seen it, declare victory and print
          printf("*** %d %d\n", i, j)
          printf("*** %d\n", i * j)
        } else {
          // otherwise add it to the hash
          hash += i
        }
      } catch {
        case e: Throwable => None
      }
  }

  /** Day 1 second challenge: find triples that add to 2020
   *
   *  O(n^3) Walk the permutations until you find one that matches.
   *  Performed fine for the data in this case. We could also sort
   *  the list first, which lets us short-circuit loops whenever
   *  we start getting sums over the target because we know it will
   *  just continue to go up from there. We could also do a pass
   *  building a hash for looking up the third value which would
   *  remove the k loop and bring this down to O(n^2)
   */
  def secondChallenge(f: BufferedSource): Unit = {
    val lines: Array[Int] = f.getLines.toList.map(x => x.toInt).toArray
    val len: Int = lines.length

    var i = 0
    var j = 1
    var k = 2

    var currentSum = 0

    // Check to see that k (which is always farther than i and j
    // hasn't ended up at the bounds, and that we haven't
    // succeeded
    while ( k < len && currentSum != TARGET ) {
      // Check for success and declare victory if possible
      currentSum = lines(i) + lines(j) + lines(k)
      if (currentSum == TARGET) {
        printf("*** %d %d %d\n", lines(i), lines(j), lines(k))
        printf("*** %d\n", lines(i) * lines(j) * lines(k))
      }
      // Otherwise, increment the innermost loop
      k += 1
      // When that hits bounds, increment the middle loop and reset k
      if (k == len) {
        j += 1
        // When THAT hits bounds, increment the the outer loop and reset j
        if (j == (len - 1)) {
          i += 1
          j = i + 1
        }
        k = j + 1
      }
    }
  }
  
  def main(args: Array[String]) {
    val base = System.getProperty("user.dir")
    val f = Source.fromFile(base + "/resources/input.txt")
//    firstChallenge(f)
    secondChallenge(f)
    f.close()
    printf("*** DONE\n")
  }

}

