package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object PpaFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val bitstringsSource = opt[String]("bitstrings", descr = "File containing bitstrings")
    val extendedFeatures = opt[Boolean]("extended",short='e', descr="Use extended features.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the PPA native format for 
 * classification.
 */
object PpaFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = PpaFeaturesOpts(args)
   
    val inputFile = opts.inputFile()

    val bitstrings = opts.bitstringsSource.get match {
      case Some(bitstringsSource) =>
        io.Source.fromFile(bitstringsSource).getLines.map { line =>
          val Array(word, bitstring) = line.split("\\s+")
          (word -> BitVector(bitstring))
        }.toMap

      case None => new collection.immutable.HashMap[String, BitVector]()
    }

    val featureExtractor =
      if (opts.extendedFeatures()) new ExtendedFeatureExtractor(bitstrings)
      else BasicFeatureExtractor

    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val Array(id, verb, noun, prep, prepObj, attach) = line.split(" ")
      val features = featureExtractor(verb, noun, prep, prepObj)
      println(features.map(_.toString).mkString(",") + "," + attach)
    }

  }

}

/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait FeatureExtractor {
  
  /**
   * Given the verb, noun, preposition, and prepositional object,
   * create a set of AttrVal objects. (A "feature" is an attribute with a
   * value.) 
   */
  def apply(verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal]
}

/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicFeatureExtractor extends FeatureExtractor {

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
    List(
      AttrVal("verb", verb),
      AttrVal("noun", noun),
      AttrVal("prep", prep),
      AttrVal("prep_obj", prepObj))
  }

}

/**
 * An extended feature extractor. It is your job to fill this out further.
 */
class ExtendedFeatureExtractor(bitvectors: Map[String, BitVector])
  extends FeatureExtractor {

  lazy val stemmer = new PorterStemmer
 
  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {

    // Use the basic feature extractor to get the basic features (no need to 
    // duplicate effort and specify it again).
    val basicFeatures = BasicFeatureExtractor(verb, noun, prep, prepObj)
   // println(bitvectors)
    // Extract more features
    val nounBitvector = new BitVector(bitvectors.getOrElse(noun,"0000000000").toString.split("").drop(1).map(_.toInt).toIndexedSeq)
    val verbBitvector = new BitVector(bitvectors.getOrElse(verb,"0000000000").toString.split("").drop(1).map(_.toInt).toIndexedSeq)
    val prepBitvector = new BitVector(bitvectors.getOrElse(prep,"0000000000").toString.split("").drop(1).map(_.toInt).toIndexedSeq)
    val prepObjBitvector = new BitVector(bitvectors.getOrElse(prepObj,"0000000000").toString.split("").drop(1).map(_.toInt).toIndexedSeq)
    val suffix = List("ative","esque","tion","sion","ship","ness","ment","less","ious","ical","ible","ence","ance","able","est","ous","ing","ize","ive","ity","ist","ism","ish","ise","ify","ful","dom","ate","acy","ty","or","ic","fy","ed","es","er","en","al","al","s","y")
    val prefix = List("under","trans","super","inter","anti","sub","semi","over","fore","pre","non","mis","mid","dis","un","re","ir","in","in","im","im","il","en","em","de")
    val noun_Vector = nounBitvector.keepTopBits(4)
    val verb_Vector = verbBitvector.keepTopBits(4)
    val prep_Vector = prepBitvector.keepTopBits(4)
    val prepObj_Vector = prepObjBitvector.keepTopBits(4)
    var ext_features = 
    List(
      AttrVal("verb_stem",stemmer(verb)),
      AttrVal("noun_stem",stemmer(noun)),
      //AttrVal("NounVerb_vector",noun_Vector.toString+verb_Vector.toString),
      AttrVal("PPO_vector",prep_Vector.toString+prepObj_Vector.toString)
      );
    if(verb.length<=5)
      ext_features=ext_features++List(AttrVal("verb_length","short"))
    else
      ext_features=ext_features++List(AttrVal("verb_length","long"))
    if (noun matches """[0-9]+([.,]?[0-9]+)*""")
      ext_features=ext_features++List(AttrVal("noun_number","number"))
    if (prepObj matches """[0-9]+([.,]?[0-9]+)*""")
      ext_features=ext_features++List(AttrVal("prepObj_number","number"))
    if (noun.exists(_.isUpper)){
      if(noun matches """[A-Z]+([.]?[A-Z]*)*""")
        ext_features=ext_features++List(AttrVal("noun_form","XX"))
      else
        ext_features=ext_features++List(AttrVal("noun_form","Xx"))
      }
    if (prepObj.exists(_.isUpper)){
      if(prepObj matches """[A-Z]+([.]?[A-Z]*)*""")
        ext_features=ext_features++List(AttrVal("prepObj_form","XX"))
      else
        ext_features=ext_features++List(AttrVal("prepObj_form","Xx"))
      }
    if((prep+prepObj).length<=10)
      ext_features=ext_features++List(AttrVal("ppo_length","short"))
    else
      ext_features=ext_features++List(AttrVal("ppo_length","long"))
    if (prepObj.contains("-"))
      ext_features=ext_features++List(AttrVal("prepObj_hyphen","-"))
    if (noun.contains("-"))
      ext_features=ext_features++List(AttrVal("noun_hyphen","-")) 
    for(word <- prefix){
        if(noun.startsWith(word))
          ext_features=ext_features++List(AttrVal("noun_prefix",word))
    }
    for(word <- suffix){
        if(verb.endsWith(word))
          ext_features=ext_features++List(AttrVal("verb_suffix",word))
    }
    ext_features=ext_features++List(AttrVal("verb_noun",verb+"+"+noun))
    ext_features=ext_features++List(AttrVal("verb_prepObj",verb+"+"+prepObj))
    // Return the features. You should of course add your features to basic ones.
   basicFeatures++ext_features
  }

}

/**
 * This is an entirely cruddy, slow implementation of a bit vector,
 * not using any bitwise ops, etc., but it should suffice for this problem.
 *
 * And, yes, we are using Ints where it could be Booleans, and we could have
 * the wrong values in there, but this keeps it easy, and again, is sufficient
 * for this problem.
 * 
 * Feel free to add more capability to this if it helps you create better
 * features.
 */
class BitVector(bits: IndexedSeq[Int]) {

  /**
   * Get the bit value at the given index.
   */
  def apply(index: Int) = bits(index)

  /**
   * Get the integer value of the bits
   */
  lazy val toInt = Integer.parseInt(bits.mkString, 2)

  /**
   *  Keep the top bits up to the given index, and then make the remaining bits
   *  zero.
   */
  def keepTopBits(index: Int) =
    new BitVector(bits.take(index) ++ Vector.fill(bits.length - index)(0))

  /**
   * Concatenate the bits together.
   */
  override def toString = bits.mkString
}

/**
 * Companion object to the BitVector class.
 */
object BitVector {

  /**
   * Create a bit vector from a string of zeros and ones.
   */
  def apply(bitstring: String) =
    new BitVector(bitstring.split("").drop(1).map(_.toInt).toIndexedSeq)
}



