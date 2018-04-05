package nltk

class Tokenizer(sents: Sentenceizer) {
  
  private[this] def get_tokens_of_sentence(sentence: String): List[String] = {
  	val words: List[String] = sentence.split("\\s+").map(_.trim).toList
  	// TODO split actual words from punctuation marks
  	words
  }

  private val sentences = sents.get_list_of_sentences()
  private val tokens_by_sequence: List[List[String]] = sentences.map(get_tokens_of_sentence(_))

  // val tokens = tokens_by_sequence.flatten

  val tokens = List("I","compute","therefore","I","am")

}
