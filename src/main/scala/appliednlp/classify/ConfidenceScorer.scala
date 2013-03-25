package appliednlp.classify

import math.{ceil,floor}
/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  def main(args: Array[String]) {
  	val predictions = io.Source.fromFile(args(1))
  	val goldfile = io.Source.fromFile(args(0))
  	lazy val labelRE = """^.*[,\s]\b([A-Z][a-z]*?)\b$""".r 
  	val goldLabel = goldfile.getLines.map{line =>
  		line match {
  			case labelRE(label) =>{
  				label
  			}
  		}
  	}.toList	
  	//println(goldLabel)
  	val pred = predictions.getLines.toList
  	//println(pred)
  	val confidence = pred.zip(goldLabel)
  	val confidenceMap = confidence.map(conf => (conf._1,conf._2)).toMap
    val sorted_predictions = confidenceMap.toSeq.sortBy(-_._1.split(' ').apply(1).toDouble)
    //val sortedMap = sorted_predictions.map(conf => (conf._1,conf._2)).toMap
    val reverseMap = confidenceMap.toSeq.sortBy(_._1.split(' ').apply(1).toDouble)
  	//confidenceMap foreach((x) => println(x._1+"->"+x._2))
  	//println(reverseMap)
  	var topAcc =0
  	var midAcc =0
  	var bottomAcc =0
  	val mapSize = sorted_predictions.size/3.0
  	for((key,value) <- sorted_predictions.slice(0, ceil(mapSize).toInt)){
  		if(key.split(' ').take(1).mkString==value)
  			topAcc += 1
  	}
  	
  	for((key,value) <- sorted_predictions.slice(ceil(2*mapSize).toInt,sorted_predictions.size)){
  		if(key.split(' ').take(1).mkString==value)
  			bottomAcc += 1
  	}
  	
  	for((key,value) <- sorted_predictions.slice(ceil(mapSize).toInt, ceil(2*mapSize).toInt)){
  		if(key.split(' ').take(1).mkString==value)
  			midAcc += 1
  	}
  	
  	println("High Confidence Accuracy: "+100*topAcc/ceil(mapSize))
  	println("Mid Confidence Accuracy: "+100*midAcc/(ceil(2*mapSize)-ceil(mapSize)))
  	println("Low Confidence Accuracy: "+100*bottomAcc/(sorted_predictions.size-ceil(2*mapSize)))
  }  

}
