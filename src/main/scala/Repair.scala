import nltk._

class Repair(filename: String) {
  // Sources for training and testing datasets
  // http://www.iesl.cs.umass.edu/datasets.html
  // https://machinelearningmastery.com/datasets-natural-language-processing/
  // http://www.nltk.org/nltk_data/

  // Potential Corpora for training and testing
  // TIMIT Acoustic Phonetic Continuous Speech Corpus (Paid)
  // Voxforge (Open Source)

  // Run the pipeline
  // May need to run twice: 1st time for training, 2nd for testing

  def repair(tok: Tokenizer, pos: POSTagger): List[String] = {
    List("a","b","c")
  }

  import scala.io.Source
  val str = Source.fromResource(filename).getLines.toList
  val sen = new Sentenceizer("Hello, World!")
  val tok = new Tokenizer(sen)
  val pos = new POSTagger(tok)
  val rep = repair(tok, pos)
  println(filename)

}
