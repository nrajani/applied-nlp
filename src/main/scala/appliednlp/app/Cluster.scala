package appliednlp.app

import org.apache.log4j.Level
import org.apache.log4j.Logger
import nak.cluster.Point
import nak.cluster.ClusterConfusionMatrix
import nak.cluster.ClusterReport
import nak.cluster.Kmeans
import nak.cluster.DistanceFunction
import nak.cluster.PointTransformer

import appliednlp.cluster._

/**
 * A standalone object with a main method for reading an input file and running
 * k-means with different options.
 */
object Cluster {
import math.{sqrt,pow}

  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = ClusterOpts(args)
    
    // This tells the k-means algorithm how much debugging information to show you
    // while you run the algorithm. 
    val logLevel = if (opts.verbose()) Level.DEBUG else Level.INFO
    Logger.getRootLogger.setLevel(logLevel)
    
    // Your code starts here. You'll use and extend it during every problem.
    // Part 1
    /*
    val pointObject = DirectCreator(opts.filename()).toList
    val pointType = pointObject.map(x => x._3).toIndexedSeq
    val kmeans = new Kmeans(points = pointType, distance = DistanceFunction(opts.distance()))
    val out= kmeans.run(k=opts.k())
    val centroidOut = kmeans.computeClusterMemberships(out._2)
    val label = pointObject.map(x =>x._2).toIndexedSeq
    val confusionMatrix = ClusterConfusionMatrix(goldClusterIds=label,numPredictedClusters=out._2.length,predictedClusterIndices=centroidOut._2)
    //println(confusionMatrix)

    // Part 2
    val zPointObject = PointTransformer("z", pointType)
    val zPoints = zPointObject(pointType)
    val zkmeans = new Kmeans(points = zPoints, distance = DistanceFunction(opts.distance()))
    val zOut= zkmeans.run(k=opts.k())
    val zCentroidOut = zkmeans.computeClusterMemberships(zOut._2)
    val zConfusionMatrix = ClusterConfusionMatrix(goldClusterIds=label,numPredictedClusters=zOut._2.length,predictedClusterIndices=zCentroidOut._2)
    //println(zConfusionMatrix)

    // Part 3
    //if(opts.features()=="schools"){
    val schoolObject = SchoolsCreator(opts.filename()).toList
    val schoolType = schoolObject.map(x => x._3).toIndexedSeq
    val schoolsKmeans = new Kmeans(points = schoolType, distance = DistanceFunction(opts.distance()))
    val schoolOut = schoolsKmeans.run(k=opts.k())
    val schoolCentroidOut = schoolsKmeans.computeClusterMemberships(schoolOut._2)
    val schoolLabel = schoolObject.map(x =>x._2).toIndexedSeq 
    val schoolConfusionMatrix = ClusterConfusionMatrix(goldClusterIds= schoolLabel, numPredictedClusters = schoolOut._2.length, predictedClusterIndices = schoolCentroidOut._2)
    println(schoolConfusionMatrix)
    ClusterReport(schoolObject.map(x => x._1).toSeq,schoolLabel,schoolCentroidOut._2)
    //}

   // Part 4
    val countryObject = CountriesCreator(opts.filename()).toList
    val countryType = countryObject.map(x => x._3).toIndexedSeq
    val countryKmeans = new Kmeans(points = countryType, distance = DistanceFunction(opts.distance()))
    val countryOut = countryKmeans.run(k=opts.k())
    val countryCentroidOut = countryKmeans.computeClusterMemberships(countryOut._2)
    val countryLabel = countryObject.map(x =>x._2).toIndexedSeq 
    val countryConfusionMatrix = ClusterConfusionMatrix(goldClusterIds= countryLabel, numPredictedClusters = countryOut._2.length, predictedClusterIndices = countryCentroidOut._2)
    println(countryConfusionMatrix)
*/
    // Part 5
    val fedCreator = new FederalistCreator
    val fedObject = fedCreator(opts.filename()).toList
    val fedType = fedObject.map(x => x._3).toIndexedSeq
    val fedKmeans = new Kmeans(points = fedType, distance = DistanceFunction(opts.distance()))
    val fedOut = fedKmeans.run(k=opts.k())
    val fedCentroidOut = fedKmeans.computeClusterMemberships(fedOut._2)
    val fedLabel = fedObject.map(x =>x._2).toIndexedSeq 
    val fedConfusionMatrix = ClusterConfusionMatrix(goldClusterIds= fedLabel, numPredictedClusters = fedOut._2.length, predictedClusterIndices = fedCentroidOut._2)
    println(fedConfusionMatrix)
    //ClusterReport(fedObject.map(x => x._1).toSeq,fedLabel,fedCentroidOut._2)
  }

}


/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object ClusterOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Cluster application using nak.cluster.Kmeans

The distance functions are standard cosine, euclidean, and manhattan.

The transform option is one ident, zscore, and pca.
 - ident: no transformation of the input points
 - zscore: use standard z-scores in each dimension to scale the points
 - pca: scale the points with z-scores, then run PCA and use (as transformed dimensions) only the principle components that explain 95% of the variance

For usage see below:
	     """)

    val featureTypes = Set("standard","schools","countries","fed-simple","fed-full")
    val transformers = Set("i","ident","z","zscore","p","pca")
    val distanceFunctions = Set("c","cosine","e","euclidean","m","manhattan")
    val transform = opt[String]("transform", default=Some("ident"), validate = transformers, descr = "The transformation to use. Possible values: " + transformers.toSeq.sorted.mkString(",") )
    val distance = opt[String]("dist", default=Some("cosine"), validate = distanceFunctions, descr = "The distance function to use. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )      
    val features = opt[String]("features", default=Some("standard"), validate = featureTypes, descr = "The type of features to extract. Possible values: " + distanceFunctions.toSeq.sorted.mkString(",") )
    val k = opt[Int]("num-clusters",short='k', required=true, validate = (0<), descr="The number of clusters to find.")
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val showCentroids = opt[Boolean]("output-centroids",short='c', descr="Show centroids.")
    val report = opt[Boolean]("output-report",short='r', descr="Show full cluster report.")
    val filename = trailArg[String]("filename", descr = "The input filename.")
  }
}
