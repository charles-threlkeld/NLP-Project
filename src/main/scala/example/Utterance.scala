package example

class Tokenizer(rawText: List[String]) {
  // Tokenize methods
  val tokens = rawText
}

class Lemmaizer(tokens: Tokenizer) {
  // Lemmaization methods
  val lemmas = tokens.tokens
}

class POSTagger(lemma: Lemmaizer) {
  // Part Of Speech Tagging Methods
  val tags = lemma.lemmas
}

class Chunk(posTagged: POSTagger) {
  // Chunking Methods
  val chunk = posTagged.tags
}

class Repair(chunks: Chunk) {
  // Utterance Repair Methods
  val repair = chunks.chunk
}

object Repair extends Utterance with App {
  // Sources for training and testing datasets
  // http://www.iesl.cs.umass.edu/datasets.html
  // https://machinelearningmastery.com/datasets-natural-language-processing/
  // http://www.nltk.org/nltk_data/

  // Potential Corpora for training and testing
  // TIMIT Acoustic Phonetic Continuous Speech Corpus (Paid)
  // Voxforge (Open Source)

  // Run the pipeline
  // May need to run twice: 1st time for training, 2nd for testing

  val utterances: List[String] = List(greeting)
  val tok = new Tokenizer(utterances)
  val lem = new Lemmaizer(tok)
  val pos = new POSTagger(lem)
  val chu = new Chunk(pos)
  val rep = new Repair(chu)
  println(rep.repair)

}

trait Utterance {
  lazy val greeting: String = "Hello, World!"
}
