package example

object Hello extends Greeting with App {
  println(greeting)

  // Sources for training and testing datasets
  // http://www.iesl.cs.umass.edu/datasets.html
  // https://machinelearningmastery.com/datasets-natural-language-processing/
  // http://www.nltk.org/nltk_data/

  // Potential Corpora for training and testing
  // TIMIT Acoustic Phonetic Continuous Speech Corpus (Paid)
  // Voxforge (Open Source)

  // Tokenization

  // Lemmaization

  // Part-of-speech tagging

  // Chunking

}

trait Greeting {
  lazy val greeting: String = "Hello, World!"
}
