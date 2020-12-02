import scala.collection.mutable.HashSet
import scala.io.BufferedSource
import scala.io.Source

object Main {

  val TARGET = 2020

  def firstChallenge(f: BufferedSource): Unit = {
    val hash: HashSet[Int] = HashSet.empty[Int]
    for (line <- f.getLines)
      try {
        val i = line.toInt
        val j = TARGET - i
        if (hash(j)) {
          printf("*** %d %d\n", i, j)
          printf("*** %d\n", i * j)
        } else {
          hash += i
        }
      } catch {
        case e: Throwable => None
      }
  }

  def secondChallenge(f: BufferedSource): Unit = {
    val lines: Array[Int] = f.getLines.toList.map(x => x.toInt).toArray
    val len: Int = lines.length

    var i = 0
    var j = 1
    var k = 2

    var currentSum = 0

    while ( k < len && currentSum != TARGET ) {
      currentSum = lines(i) + lines(j) + lines(k)
      if (currentSum == TARGET) {
        printf("*** %d %d %d\n", lines(i), lines(j), lines(k))
        printf("*** %d\n", lines(i) * lines(j) * lines(k))
      }
      k += 1
      if (k == len) {
        j += 1
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

