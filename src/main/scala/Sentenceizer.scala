package nltk

class Sentenceizer(filename: String) {

  import scala.io.Source

  private val file: Source = Source.fromResource(filename)

  private val rawText: String = file.getLines.mkString

  private val rawSentences: Array[String] = rawText.split(Array('\n', '.', '?', '!'))

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
