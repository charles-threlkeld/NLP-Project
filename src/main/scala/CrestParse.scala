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

    def slicer(text: String, id: String): String = {
      val sliceBegin = text.indexOf(id) + id.length
      val sliceEnd = (text.slice(sliceBegin, text.length)).indexOf("\"")
      return text.slice(sliceBegin, sliceBegin + sliceEnd)
    }

    import scala.io.Source
    val file = Source.fromResource(filename)
    val lines = file.getLines.toList
    val tiers = lines.filter(_ contains "<tier ")
    val tierIds = tiers.map(slicer(_, "display-name="))
    


    file.getLines.toList.filter(_ contains "start=")
  }

  // Transform xml to text and timestamps
  // Input: a single line of xml (<event ...>Text</event>)
  // Output: (Text, Begin-time, End-time)
  def process_xml_lines(xml_list: List[String]): (Int, Int, String) = {

    // This helper function will get the values of any id tag in an xml line
    // It does not handle tag not found errors, though so be careful.
    // Input: xml_line: a String in the xml format
    // Input: tag: the tag string to find, e.g. "display-name"
    // Output: String: the tag value, e.g. "DirectorS1 [v]"
    def get_tag(xml_line: String, tag: String): String = {
      val expanded_tag = tag + "=\""
      val tagStart = (xml_line indexOf expanded_tag) + expanded_tag.length + 2
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
    val tier_labels = tier_lines map (get_tag(_, "display-name"))

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










    // Then, we can tag the utterances by following the start/end tags
    // as linked lists
    // E.g. an utterance may have start="T5" end="T10", so we find the 
    // token with start="T5", and add it to a list.
    // Then check the token's end tag. If it is "T10", then stop adding
    // tokens to the list
    // If it is not "T10", find the token that has start="T10" and recur.
    // After finding stop="T10", find the part-of-speech tags that have 
    // been tagged for the tokens of the utterance.
    // Finally, also tag any disfluencies if they occur, in the appropriate
    // corpus files (i.e. '08 and '10 files)

    // Do this for each utterance in the tiers Director and Speaker

  }

  val corpus = List("Muri_07_S3_merged.xml",
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

  val filename = "Muri_07_S3_merged.xml"
  val lines = get_file(filename)
  val processed_lines = process_xml_lines(lines)
  // TODO: Match words to POS tags
  // Match word-tag tuples to utterances
}

