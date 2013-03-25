package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer
import scala.collection.mutable
import scala.math.log


object TextClassify{
		def main(args: Array[String]) {
			if(args(0)=="-A"){
				val adultFeatureExtractor = new AdultFeatureExtractor
				val inputFile = io.Source.fromFile(args(1)).getLines.foreach { line =>
      			val Array(age, workclass, fnlwgt, education, education_num, marital_status, occupation, relationship, race, sex, capital_gain, capital_loss, hour_per_week, native_country, attach) = line.split(" ")
      			val features = adultFeatureExtractor(age, workclass, fnlwgt, education, education_num, marital_status, occupation, relationship, race, sex, capital_gain, capital_loss, hour_per_week, native_country)
      			println(features.map(_.toString).mkString(",") +","+ attach)
    		}

    	}
    		else{
    			lazy val stopwords = io.Source.fromFile("stopwords.english").getLines().toSet
    			val newsFeatureExtractor = new NewsFeatureExtractor	
    			val newsRE = """^([a-z]+(?:\.[a-z]+[-]?[a-z]*)+)\t([0-9]+)\t(.*)$""".r
    			val idfDoc = io.Source.fromFile(args(0)).getLines.toList
    			io.Source.fromFile(args(0)).getLines.foreach { line =>
    				val newsMap = line match {
  						case newsRE(label,number,text) => val cleanText = SimpleTokenizer(text).filter(y=> stopwords.contains(y) == false).mkString(" ")
  						Map("newsLabel"->label,"docNumber"->number,"newsText"->cleanText)
  							}
  					val features = newsFeatureExtractor(newsMap,idfDoc)
  					println(features.map(_.toString).mkString(",") +","+ newsMap.get("newsLabel"))
  				}
  				
    		}
	}
}

class AdultFeatureExtractor{
	def apply(age: String, workclass: String, fnlwgt: String, education: String, education_num: String, marital_status: String, occupation: String, relationship: String, race: String, sex: String, capital_gain: String, capital_loss: String, hour_per_week: String, native_country: String): Iterable[AttrVal] = {
    var features = 
    List(
      AttrVal("age", age),
      AttrVal("workclass", workclass),
      AttrVal("fnlwgt", fnlwgt),
      AttrVal("education", education),
      AttrVal("education_num", education_num),
      AttrVal("marital_status", marital_status),
      AttrVal("occupation", occupation),
      AttrVal("relationship", relationship),
      AttrVal("race", race),
      AttrVal("sex", sex),
      AttrVal("capital_gain", capital_gain),
      AttrVal("capital_loss", capital_loss),
      AttrVal("hour_per_week", hour_per_week),
      AttrVal("native_country", native_country)
      )
    if(age.toInt<=30)
    	features=features++List(AttrVal("age_class","young"))
    else if(age.toInt>30 && age.toInt<=50)
    	features=features++List(AttrVal("age_class","mid_age"))
    else
    	features=features++List(AttrVal("age_class","old"))
    features
  }
}

class NewsFeatureExtractor{
	def apply(newsMap: Map[String,String],idfDoc:List[String]):Iterable[AttrVal]={
		lazy val stopwords = io.Source.fromFile("stopwords.english").getLines().toSet
		val idfCounts = mutable.Map[String, Int]().withDefault(x=>0)

			val wordText = newsMap.get("newsText").mkString(" ")
			var tfCount = wordText.toLowerCase.split("\\s+").toSeq.groupBy(y => y).mapValues(_.length).toMap

			for(k<- tfCount){
				for(doc<- idfDoc){
					if(doc.contains(k))
						idfCounts(k._1)+=1
						}
			}
			val tfidf = tfCount.map(x => (x._1,(x._2:Int)*(idfCounts.getOrElse(x._1,0):Int)))
		
			val features = tfCount.map(x => AttrVal(x._1,x._2.toString)).toList

		features

	}
}

object SimpleTokenizer {
  def apply(text: String): IndexedSeq[String] = text
    .replaceAll("""([\?!()\";\|\[\].,'])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
}