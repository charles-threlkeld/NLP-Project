package nltk

class Repair(filename: String) {
  // Sources for training and testing datasets
  // resources/train.txt
  // resources/test.txt
  // Derived from the CReST Corpus

  // Run the pipeline

  type rePair = Array[String] // repairable string and its repaired candidate

  private def get_file(filename: String): List[rePair] = {

    import scala.io.Source
    val file = Source.fromResource(filename)
    val lineList = file.getLines.toList
    lineList map (_.split(";"))

  }

  // This is our testing method. It takes a list of algorithmically repaired
  // utterances and a list of hand-repaired utterances and calculates
  // their similarity using the Dice algorithm (2*intersection) / (|s1| + |s2|)

  // Input: candidates: List[List[String]]: our list of algorithmically
  //                                        repaired utterances in a
  //                                        tokenized format
  //        control: List[List[String]]: Hand-repaired utterances in a
  //                                     tokenized format
  // Output: Double: The average dice score. A perfect score is 1
  
  private def test_with_dice(unrepaired: Tokenizer, repaired: Tokenizer): Double = {

    val candidates = unrepaired.tokens_by_sequence
    val control = repaired.tokens_by_sequence

    val intersections: List[Double] = (for (i <- 0 to candidates.length - 1) yield {
      // Note the intersection is doubles as part of the calculation
      // for the dice score, so this is not the true size of the
      // intersection and cannot be reused as such

      (candidates(i).intersect(control(i))).length * 2.0
    }).toList

    val sum_magnitudes: List[Double] = (for (i <- 0 to candidates.length - 1) yield {
      candidates(i).length + control(i).length * 1.0
    }).toList

    val quotients: List[Double] = (for (i <- 0 to candidates.length - 1) yield {
      intersections(i) / sum_magnitudes(i)
    }).toList

    quotients.sum / quotients.length
  }

  // This method is to get our baseline dice score
  // Input: List[rePair]: Our raw list onf unrepaired and hand-repaired
  //                      utterances from train.txt or test.txt
  // Output: Double: The average Dice score, from which we hope to improve
  private def baseline(input: List[rePair]): Double = {

    val unrepaired: List[String] = for (i <- input) yield i(0)
    val repaired: List[String] = for (i <- input) yield i(1)

    val unrepairedSentences = new Sentenceizer(unrepaired.toArray)
    val repairedSentences = new Sentenceizer(repaired.toArray)

    val unrepairedTokens = new Tokenizer(unrepairedSentences)
    val repairedTokens = new Tokenizer(repairedSentences)

    test_with_dice(unrepairedTokens, repairedTokens)
  }

  // train uses the features from Feature.featureList to attempt to repair
  // utterances for disfluencies
  // Input: None (implicitly the filename with the utterances and hand-repair
  // Output: Double - the average dice score
  private def train(): Double = {

    val unrepaired: List[String] = for (i <- input) yield i(0)
    val unrepairedSentences = new Sentenceizer(unrepaired.toArray)
    val repairedSentences = repair(unrepairedSentences)
    val unrepairedTokens = new Tokenizer(unrepairedSentences)
    val repairedTokens = new Tokenizer(repairedSentences)

    val control = for(i <- input) yield i(1)
    val controlSentences = new Sentenceizer(control.toArray)
    val controlTokens = new Tokenizer(controlSentences)

    test_with_dice(repairedTokens, controlTokens)
  }

  // This method is where the magic happens
  // Input: utterance: Sentenceizer - our array of disfluent utterances
  // Output: Sentenceizer - the array of repaired utterances0
  def repair(utterance: Sentenceizer): Sentenceizer = {
    // TODO
    utterance
  }

  private val input = get_file(filename)
  val base = baseline(input)
  val repScore = train()

}
