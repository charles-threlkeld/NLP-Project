package nltk

class Sentenceizer(rawText: String) {
  private val sentences: Array[String] = rawText.split("\n")

  private[this] def validate_sentences(): Array[String] = {
  	// TODO 
  	sentences
  }

  validate_sentences()

  def get_list_of_sentences(): List[String] = sentences.toList

  def get_sentence_by_number(sentence_no: Int) = sentences(sentence_no - 1)
}
