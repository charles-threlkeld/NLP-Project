package nltk

// This class extracts the raw spoken text
// from a given file of the CReST Corpus
// It wraps them in a Sentencizer object
// which can then be part-of-speech tagged
// and given to the repair module

class CrestParse(filename: String) {


  // Load a corpus file
  // Input: A filename (e.g. "Muri_07_S3_merged.xml")
  // Output: A list of the lines in the file
  def get_file(filename: String):List[String] = {

    import scala.io.Source
    val file = Source.fromResource(filename)
    val lines = file.getLines.toList
    val tiers = lines.filter(_ contains "<tier ")

    file.getLines.toList.filter(_ contains "start=")
  }

  // Transform xml to text and timestamps
  // Input: a single line of xml (<event ...>Text</event>)
  // Output: (Text, Begin-time, End-time)
  def process_xml_lines(xml_list: String) (Int, Int, String) = {

    val len = xml_list.length
    val firstTimestampStart = xml_list indexOf "T" + 1
    val firstTimestampEnd = xml_list.substring(firstTimestampStart, len) indexOf "\""
    val t1 = xml_list.substring(firstTimestampStart, firstTimestampStart + firstTimestampEnd)

    val truncatedXml = xml_list.substring(firstTimestampStart + firstTimestampEnd, len)
    val len_trunc = truncatedXml.length
    val secondTimestampStart = (truncatedXml indexOf "T") + 1
    val secondTimestampEnd = truncatedXml.substring(firstTimestampStart, len) indexOf "\""
    val t2 = truncatedXml.substring(secondTimestampStart, secondTimestampStart + secondTimestampEnd)

    val textBegin = (truncatedXml indexOf ">") + 1
    val textEnd = truncatedXml indexOf "<"
    val text = truncatedXml.substring(textBegin, textEnd)

    return (t1, t2, text)
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
  val processed_lines = lines map process_xml_lines
  // TODO: Match words to POS tags
  // Match word-tag tuples to utterances


}

