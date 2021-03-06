package appliednlp.cluster

import nak.cluster._
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer
import scala.collection.immutable
import scala.collection.mutable

import org.apache.log4j.Logger
import org.apache.log4j.Level
import scala.io.Source
import math.log

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])

/**
 * Read data in the standard format for use with k-means.
 */
 object PointCreator {

    def apply(feature: String) = feature match {
    case "standard" => DirectCreator
    case "schools" => SchoolsCreator
    case "countries" => CountriesCreator
    case "fed-simple" => new FederalistCreator(simple=true)
    case "fed-full" => new FederalistCreator(simple=false)
  }
}

object DirectCreator extends PointCreator {

 def apply(filename: String) = Source.fromFile(filename).getLines.collect(_.split("\\s+").toList match {
      case s1 :: s2 :: points => (s1, s2, Point(points.map(_.toDouble).toIndexedSeq))
    })
 //List[(String,String,Point)]().toIterator
}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */
object SchoolsCreator extends PointCreator {
  def apply(filename: String) = Source.fromFile(filename).getLines.flatMap(_.split("\\s+").toList match {
      case s1 :: p1x :: p1y :: p2x :: p2y :: Nil => List( (s1,"4",Point(List(p1x,p1y).map(_.toDouble).toIndexedSeq)),
          (s1,"6",Point(List(p2x,p2y).map(_.toDouble).toIndexedSeq)) )
      case s1 :: s2 ::p1x :: p1y :: p2x :: p2y :: Nil => List( (s1+"_"+s2,"4",Point(List(p1x,p1y).map(_.toDouble).toIndexedSeq)),
          (s1+"_"+s2,"6",Point(List(p2x,p2y).map(_.toDouble).toIndexedSeq)) )
                                   // => (s1,"6",Point(points2.map(_.toDouble).toIndexedSeq))
  //List[(String,String,Point)]().toIterator
  })
}

/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {
  def apply(filename: String) = Source.fromFile(filename).getLines.flatMap(_.split("\\s+").toList match {
      case s1 :: p1x :: p1y :: Nil => List( (s1,"1",Point(List(p1x,p1y).map(_.toDouble).toIndexedSeq)))
      case s1 :: s2 ::p1x :: p1y :: Nil => List( (s1+"_"+s2,"1",Point(List(p1x,p1y).map(_.toDouble).toIndexedSeq)))
                                   // => (s1,"6",Point(points2.map(_.toDouble).toIndexedSeq))
  //List[(String,String,Point)]().toIterator
  })
}

/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) = {
    val fedArticles = FederalistArticleExtractor(filename)
    val text = fedArticles.flatMap(_.get("text"))
    val fedId = fedArticles.flatMap(_.get("id"))
    val fedAuthor = fedArticles.flatMap(_.get("author"))
    val fedText = if(simple) extractSimple(text) else extractFull(text)
    (fedId,fedAuthor,fedText).zipped.toIterator

  }
  //List[(String,String,Point)]().toIterator

  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    val tokens = texts.map(x => SimpleTokenizer(x.toLowerCase))
    val theCount = tokens.map(x => x.count(_ == "the")).map(_.toDouble)
    val peopleCount = tokens.map(x => x.count(_ == "people")).map(_.toDouble)
    val whichCount = tokens.map(x => x.count(_ == "which")).map(_.toDouble)
   for( i <- 0 to 84) yield Point(IndexedSeq(theCount(i),peopleCount(i),whichCount(i)))
  }

  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */

   def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    val functionWord = Source.fromFile("data/functionword.txt").getLines.toList
    val nounList = List("government","state","congress")
    val rawTokens = texts.flatMap(x => SimpleTokenizer(x.toLowerCase))
    val filteredTokens = rawTokens.filter(functionWord.contains(_) == true)
    val functionMap = mutable.Map[String, Int]().withDefaultValue(0)
    filteredTokens.foreach { token => functionMap(token) += 1 }
    functionMap.toMap
    val maxWords = functionMap.toList.sortBy{_._2}.takeRight(5).map(_._1).toIndexedSeq
    val idf = log(85/texts.count(_.contains("Macedon")))+1
   // println(idf)
    texts.map{ text => 
    val tokens = SimpleTokenizer(text.toLowerCase)
    val tf = tokens.count(_.contains("Macedon"))+1
    val tf_idf = tf*idf
    val totalCount =  tokens.filter(_ != """([\?!()\";\|\[\].,'])""").toList.distinct
    val totalFreq = totalCount.length.toDouble
    val functionCount = for( i <- 0 to maxWords.length-1) yield tokens.count(_ == maxWords(i))
    val relativeFreq = functionCount.map(x => x/totalFreq)
    val features = new immutable.VectorBuilder[Double]()
    val maxPoints = functionCount.map(_.toDouble).toIndexedSeq
    //println(maxPoints)
    features ++= maxPoints
    val wLength = tokens.filter(_ != """([\?!()\";\|\[\].,'])""").map(_.length)
    val avgLength = wLength.sum.toDouble / wLength.length
    features += avgLength
    features += tf_idf
    features ++= relativeFreq 
    //println(features)
    Point(features.result)
      }
    } 

}

object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}
