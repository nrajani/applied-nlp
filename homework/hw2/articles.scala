// Part (a)
import scala.io.Source
val book = Source.fromFile(args(0)).mkString
val JustTextRE = """(?s).*\*\*\*\sSTART\s.*\s\*\*\*(.+)\*\*\*\sEND\s.*\*\*\*.*""".r
//Pattern pattern = Pattern.compile("'(.*?)'");
//val text = JustTextRE.pattern.matcher(book);
//if (text.find())
//{
  //  System.out.print(text.group(1));
//}
var text = book match {
	case JustTextRE(mtext) => mtext
	case _ => "No Match"  
}
//println(text)
val R = """FEDERALIST(\.?)"""
//"""FEDERALIST(\.?)\sNo\.\s\d?\d"""
var rawArticles = text.split(R).toArray
//for(k<- 0 to rawArticles.length-1)
//println(rawArticles(k))

// Part (c)
val ArticleRE = """(?s)\s?No\.\s(\d?\d).*""".r
//"""FEDERALIST(?:\.?)\sNo\.\s(\d?\d)""".r
val noMatch = ""
var articles:Array[Map[String,String]] = rawArticles map(x => matching(x))
/*val articles = rawArticles map (x => x match {

  case ArticleRE(id) => (x,id)
  case _             => (x, noMatch)
}) toMap*/
def matching(word:String)={
	val noMatch = ""
	word match {
		case ArticleRE(id) => Map("id"->id)
		case _ => Map(word -> noMatch)
	}
	
}
println(articles.size)
for(j<- 0 to articles.length-1)
println(articles(j))

// Part (d)


