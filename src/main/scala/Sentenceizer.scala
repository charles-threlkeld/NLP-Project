package nltk

class Sentenceizer(rawSentences: Array[String]) {

  // Altered so that we can use this with data structures instead
  // of filenames. Moved the filename parsing to Main.scala for
  // testing

  private[this] def validate_sentences(sents: Array[String]): Array[String] = {
  	  def is_valid_sentence(sent: String): Boolean = {
  	  	  return (sent.length() > 1)
  	  }

  	  val v_sents = sents.filter(is_valid_sentence)

  	  return v_sents
  }

  private val sentences: Array[String] = validate_sentences(rawSentences)

  def get_list_of_sentences(): List[String] = sentences.toList

  def get_sentence_by_number(sentence_no: Int) = sentences(sentence_no - 1)
}
