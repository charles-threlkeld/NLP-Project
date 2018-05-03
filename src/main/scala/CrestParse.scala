package nltk

// This class extracts the raw spoken text
// from a given file of the CReST Corpus
// It wraps them in a Sentencizer object
// which can then be part-of-speech tagged
// and given to the repair module

class CrestParse() {


  // Load a corpus file
  // Input: A filename (e.g. "Muri_07_S3_merged.xml")
  // Output: A list of the lines in the file
  def get_file(filename: String): List[String] = {

    import scala.io.Source
    val file = Source.fromResource(filename)
    file.getLines.toList

  }

  type CorpusMap = List[(String,
    Map[String, List[String]])]
  type Event = (String, String, String)

  // Transform xml to text and timestamps
  // Input: a single line of xml (<event ...>Text</event>)
  // Output: (Text, Begin-time, End-time)
  def process_xml_lines(xml_list: List[String]): CorpusMap = {

    // This helper function will get the values of any id tag in an xml line
    // It does not handle tag not found errors, though so be careful.
    // Input: xml_line: a String in the xml format
    // Input: tag: the tag string to find, e.g. "display-name"
    // Output: String: the tag value, e.g. "DirectorS1 [v]"
    def get_tag(xml_line: String, tag: String): String = {
      val expanded_tag = tag + "=\""
      val tagStart = (xml_line indexOf expanded_tag) + expanded_tag.length
      if (tagStart == -1)
        ""
      else {
        val endOfLine = xml_line.slice(tagStart, xml_line.length)
        endOfLine.slice(0, endOfLine indexOf "\"")
      }
    }

    // This function will get the content of an xml line.
    // That is, the data that is within the markup
    // This isn't the most robust function, but it should serve for our corpus
    // Input: xml_line: a String in xml format, e.g. "<event>Andy</event>
    // Output: String: the data, e.g. "Andy"
    def get_data(xml_line: String): String = {
      val prefix = xml_line.slice(0, xml_line lastIndexOf "/")
      prefix.slice((prefix lastIndexOf ">") + 1, prefix.length - 1)
    }

    // Idea:
    // Each xml doc has the information embedded into tiers that are
    // individuated by the display-name ID, where S# is the number of 
    // the speaker within the corpus
    // Tiers have these display-name IDs:
    // DirectorS3: utterances, e.g. "are you there?"
    // DirectorS3WORD: tokens, e.g. "are"
    // MemberS3: utterances, e.g. "can you hear me?"
    // MembeerS3WORD: tokens, e.g. "can"
    // POS: part-of-speech, e.g. "VBP"
    // Dependency: ??, e.g. "ROOT"
    // Constituency: utterance phrase, e.g. "NP"

    // '08 files include the following additional tiers:
    // Director_Disfluency: disfluency type, e.g. "FP-B"
    // Member_Disfluency: disfluency type, e.g. "SP-M"

    // '10 files include the above plus these additional tiers:
    // Director UH/UM: non-word disfluencies, e.g. "UM-B"
    // Searcher UH/UM: non-word disfluencies, e.g. "SP-B"
    
    val (tier_lines, indices) = xml_list.zipWithIndex.filter(_._1 contains "<tier ") unzip
    val tier_labels = (tier_lines map (get_tag(_, "display-name"))).map(_.toLowerCase)

    // Each tier has a list of events that occured within that spec.
    // Each event has a start timestamp and an end timestamp
    // After gathering the indices of each tier start, we can segment 
    // the corpus into event types, and distinct sublists according
    // the the tiers

    val slice_boundaries = indices ++ List(tier_labels.length)
    val event_groups = for (i <- (0 to (indices.length - 1))) yield {
      xml_list.slice(slice_boundaries(i) + 1, slice_boundaries(i + 1))
    }

    // We can process each event to be un a usable format with start and
    // end tags and the information, but without xml decoration
    // Each event can be represented as a tuple (start, end, text)
    // Note that start and end are still strings of the form "Tx" where
    // x is some natural number. We could cast these to ints if needed.

    val clean_events = for (g <- event_groups) yield {  // For each group
      val es = for (e <- g) yield {                              // For each event
        if (e contains "<event ") {
          val start = get_tag(e, "start")
          val end = get_tag(e, "end")
          val text = get_data(e)
          (start, end, text)
        } else null
      }
      es.filter(_ != null)
    }

    // Next, we link the tiers to their respective events, so that we can
    // reference each one later when linking utterances to tokens, part-of-
    // speech, and disfluencies

    val xml_map = tier_labels zip clean_events toMap

    for (k <- xml_map.keys) {
      val l = xml_map(k).length
    }

    // So, now we have a scala representation of the important parts of our
    // xml corpus in the following form:
    // Map (tier-display-name ->
    //        List(start, end, event-data)

    // Let's get a list of all utterances:

    val dDNs = tier_labels.filter(_ contains "director") // Director Display Names
    val directorUtteranceDisplayName = dDNs(dDNs.map(_.length) indexOf dDNs.map(_.length).min)
    val directorUtterances = xml_map(directorUtteranceDisplayName)
    val mDNs = tier_labels.filter(_ contains "member") ++ tier_labels.filter(_ contains "searcher")

    val memberUtteranceDisplayName = mDNs(mDNs.map(_.length) indexOf mDNs.map(_.length).min)
    val memberUtterances = xml_map(memberUtteranceDisplayName)
    val utterances = directorUtterances ++ memberUtterances // List of all utterance events

    // Similarly, let's conglomerate tokens and disfluencies into one list each:

    val directorTokens = xml_map(dDNs.filter(_ contains "word")(0))
    val memberTokens = xml_map(mDNs.filter(_ contains "word")(0))
    val tokens = directorTokens ++ memberTokens
    val tokenCount = tokens.length

    val partsOfSpeech = xml_map(tier_labels.filter(_ contains "pos")(0))

    // break here if the information we are looking for is missing
    val missingDisfluency = dDNs.filter(_ contains "disfluency").isEmpty
    if (missingDisfluency) return null
    
    val directorDisfluencies = xml_map(dDNs.filter(_ contains "disfluency")(0))

    val memberDisfluencies = xml_map(mDNs.filter(_ contains "disfluency")(0))
    val disfluencies = directorDisfluencies ++ memberDisfluencies

    // Finally, we can map utterances to their tokens, parts of speech, and
    // disfluencies

    val corpus_map: CorpusMap = for(u <- utterances) yield {

      // tokens and POS tags can follow the start/stop chain
      def get_tokens_in_range(start: String, end: String): List[Event] = {

        // We can build the lists incrementally, starting from empty
        def builder(current: String, seeking: String,
          iter: List[Event]): List[Event] = {
          if (current == seeking)
            return iter
          else {
            val nextList = tokens.filter(_._1 == current)
            if (nextList.isEmpty) return null else {
              val next = tokens.filter(_._1 == current)(0)
              val next_end = next._2
              return builder(next_end, seeking, next :: iter)
            }
          }
        }
        builder(start, end, List())
      }

      val start = u._1
      val end = u._2
      val tokenList = get_tokens_in_range(start, end)
      if (tokenList == null) {
        null  // for empty utterances
      } else {

        // Since disfluencies are relatively rare, we only tag them when present
        // else null
        def matcher(e: List[Event], current: Event): String = {
          val d = e.filter(_._1 == current._1)
          if (d == List())
            return null
          else
            return d(0)._3
        }

        val pOSList = tokenList.map(matcher(partsOfSpeech, _))
        val disfluencyList = tokenList.map(matcher(disfluencies, _))

        // Strip tokens and POS to only the data, since we don't need the start-stop
        // values any more
        val simpleTokens: List[String] = tokenList.map(_._3)

        // The final tuple for each utterance
        val t = (u._3, Map("tokens" -> simpleTokens,
          "POS" -> pOSList,
          "disfluencies" -> disfluencyList))
        println(t)
        t
      }
    }
    return corpus_map
  }

  // This function will find all the utterances in the corpus map
  // that have been tagged with disfluencies. These disfluencies can then
  // be marked for repair both by hand for the testing group, and
  // algorithmically by the broader program
  // Input: CorpusMap as generated by the process_xml_lines function
  // Output: List[String] of the disfluent utterances
  def find_repairable_utterances(): List[String] = {

    val candidates = for (u <- corpus_map) yield {

      val disfluencies = u._2("disfluencies").filter(_ != null)
      if (disfluencies.length == 0)
        null
      else
        u._1
    }
    candidates.filter(_ != null)
  }

  val corpus = List(
    "Muri_07_S3_merged.xml",
    "Muri_07_S4_merged.xml",
    "Muri_07_S5_merged.xml",
    "Muri_07_S6_merged.xml",
    "Muri_07_S7_merged.xml",
    "Muri_08_S1_merged.xml",
    "Muri_08_S2_merged.xml",
    "Muri_08_S3_merged.xml",
    "Muri_08_S4_merged.xml",
    "Muri_08_S5_merged.xml",
    "Muri_08_S7_merged.xml",
    "Muri_10_S2_merged.xml",
    "Muri_10_S3_merged.xml",
    "Muri_10_S4_merged.xml",
    "Muri_10_S5_merged.xml",
    "Muri_10_S6_merged.xml",
    "Muri_10_S7_merged.xml",
    "Muri_10_S8_merged.xml",
    "Muri_10_S9_merged.xml",
    "Muri_10_S10_merged.xml",
    "Muri_10_S11_merged.xml",
    "Muri_10_S1_merged.xml")

  // corpus map takes the following form:
  // List(Map("utterance"    -> String
  //          "tokens"       -> List(String)
  //          "POS"          -> List(String)
  //          "disfluencies" -> List(String))
  def builder(c: List[String], iter: CorpusMap): CorpusMap = {
    if (c.isEmpty)
      return iter
    else {
      println(c.head)
      builder(c.tail, process_xml_lines(get_file(c.head)))
    }
  }

  val corpus_map = builder(corpus, List())
}
