package nltk

class POSTagger(toks: Tokenizer) {

    // Input: The filename of the testing or training file
    // Output: 2-tuple:
    //         List[String] of words in the training set
    //         List[String] of their associated POS tags
    def reformat_file(filename: String): (List[String], List[String]) = {

      def get_penn(line: String): String = {
        try { line.split(" ")(1).trim }
        catch { case e: Exception => "BLANK" }
      }
      import scala.io.Source
      val file = Source.fromResource(filename)
      val lines = file.getLines.toList
      val words = lines.map(_.split(" ")(0))
      val penn = lines map get_penn
      (words, penn)
    }

  // Training method for the POS Tagger
  // Input: filename of the corpus
  // Output: Map[String,Map[String,String]]
  //         Word -> Prev-POS -> This-POS
  def train(filename: String): Map[String,Map[String,String]] = {

    // Input: String - A single word in the corpus
    // Output: List[(String, String)] List of pairs:
    //              (prev-word-tag, this-word-tag)
    def tag_distinct(distinct_word: String, tagged_words: List[(String, String)]): Map[String,String] = {

      // get all tags for this specific word
      val (_, distinct_tags) = (tagged_words filter (_._1 == distinct_word)).unzip

      if(distinct_tags.forall(x => x == distinct_tags(0))) {
        // If a word only has one tag
        Map("VERB" -> distinct_tags(0),
          "NOUN" -> distinct_tags(0),
          "OTHER" -> distinct_tags(0))
      } else {
        // get the previous word's tag
        // send back a map of the most common prev-tag to this-tag pairs
        // TODO Handle beginning of a sentence

        val (_, indices) = tagged_words.zipWithIndex
          .filter(_._1._1 == distinct_word)
          .unzip
        val prev_indices = indices map (_ - 1)

        val prev_tags = prev_indices map (tagged_words(_)._2)

        val tag_pair = prev_tags zip distinct_tags

        val x = distinct_tags.groupBy(identity).maxBy(_._2.size)._1

        Map("VERB" -> x,
          "NOUN" -> x,
          "OTHER" -> x)
      }
    }

    val (words, tags) = reformat_file(filename)
    val distinct_words = words.distinct

    distinct_words zip (distinct_words map (tag_distinct(_, (words zip tags)))) toMap
  }

  def test(filename: String): Unit = {
    val model = train("train.txt")

    val (words, tags) = reformat_file("test.txt")

  }

  val tag_map = train("train-pos.txt")

  val s = toks.tokens

  val tags = s.map(tag_map(_))
}
