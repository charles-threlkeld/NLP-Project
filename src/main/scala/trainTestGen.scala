package nltk

// This class exists solely to read the repairable and repaired
// utterances, pair them, shuffle them, and output the pairs
// as randomized training and testing sets
// It should only be run once, but exists as documentation
// and can be run again if a new dataset is found to train
// the repair module

// Output files are of the form "repairable utterance";"repaired utterance"\n

class trainTestGen() {

  private def get_file(filename: String): List[String] = {

    import scala.io.Source
    val file = Source.fromResource(filename)
    file.getLines.toList

  }

  type trainOrTest = List[(String, String)]

  private def shuffleUtterances(candidates: List[String],
    solutions: List[String]): (trainOrTest, trainOrTest) = {

    val solvedPairs = candidates zip solutions
    val numPairs = solvedPairs.length

    import scala.util.Random.shuffle

    val shuffledPairs = shuffle((0 until numPairs).toList).toIterator

    val trainIndices = for (i <- 0 to (numPairs * 7 / 10 - 1)) yield shuffledPairs.next
    val testIndices = for (i <- (numPairs * 7 / 10) to numPairs - 1) yield shuffledPairs.next

    val train = (trainIndices map solvedPairs.apply).toList
    val test = (testIndices map solvedPairs.apply).toList

    return (train, test)
  }

  private def outputSet(dataset: trainOrTest, filename: String): Unit = {

    import java.io._

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename)))
    for (x <- dataset) {
      val repairable = x._1
      val repaired = x._2
      writer.write(s"$repairable;$repaired")
    }
    writer.close()
  }

  private val candidateFile = "repairable-utterances.txt"
  private val solutionFile = "repaired-utterances.txt"

  private val candidates = get_file(candidateFile)
  private val solutions = get_file(solutionFile)

  private val (train, test) = shuffleUtterances(candidates, solutions)

  def generate(): Unit = {
    val t = outputSet(train, "train.txt")
    val u = outputSet(test, "test.txt")
  }
}
