package nltk

class Tokenizer(sents: Sentenceizer) {
  
  private[this] def get_tokens_of_sentence(sentence: String): List[String] = {

  	val words: Array[String] = sentence.split(Array(' ', ',', ';')).map(_.trim)

  	return words.toList

  }

  private val sentences = sents.get_list_of_sentences()

  val tokens_by_sequence: List[List[String]] = sentences.map(get_tokens_of_sentence(_))

  val tokens = tokens_by_sequence.flatten

}
