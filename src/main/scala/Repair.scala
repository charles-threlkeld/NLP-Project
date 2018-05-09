import nltk._

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

  private def test_with_dice(candidates: List[List[String]],
    control: List[List[String]]): Double = {

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

  def repair(tok: Tokenizer, pos: POSTagger): List[String] = {
    List("a","b","c")
  }

  import scala.io.Source
  val str = Source.fromResource(filename).getLines.toList
  val sen = new Sentenceizer(Array("Hello, World!"))
  val tok = new Tokenizer(sen)
  val pos = new POSTagger(tok)
  val rep = repair(tok, pos)
  println(filename)

}
