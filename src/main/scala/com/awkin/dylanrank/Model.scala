package com.awkin.dylanrank

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

class Model {
    import FeatureType._
    val logger = LoggerFactory.getLogger(classOf[Model])

    def loadModelFromFile(): Map[FeatureType, Double] = {
        logger.info("load model from {}", Config.modelData)

        val (_, model: Map[FeatureType, Double], _) = 
        try {
            val lines = scala.io.Source.fromFile(Config.modelData).getLines
            ((0, Map[FeatureType, Double](), Array[Double]()) /: lines) { 
            (idx_feaMap_vector, line) =>
                val (idx, feaMap, vector) = idx_feaMap_vector
                line match {
                case BeVector() =>
                    //return the weight vector string
                    (idx, feaMap,  BeVector(line))
                case BeFeatureType() =>
                    (
                        idx + 1, 
                        feaMap + (BeFeatureType(line) -> vector(idx)), 
                        vector
                    )
                case _ =>
                    (idx, feaMap, vector)
                }
            }
        } catch {
            case ex =>
                logger.warn(ex.getMessage)
                logger.warn("unable to read model from {}", Config.modelData)
                logger.warn("use default model")
                //TODO: default model
        }
        model
    }
}

object FeatureType extends Enumeration {
    type FeatureType = Value
    val Uid, Oid, LengthTitle, LengthContent, 
        LengthDesc, Source, Unknown = Value
}

object BeVector {
    def unapply(v: String): Boolean = 
        v.length>0 && v(0)=='(' && v(v.length-1)==')'

    def apply(v: String): Array[Double] = {
        val wArray = v.substring(1, v.length-1).split(",")
        (List[Double]() /: wArray) { (wList, sValue) =>
            wList ++ List[Double](sValue.toDouble)
        }.toArray
    }
}

object BeFeatureType {
    import FeatureType._

    def unapply(f: String): Boolean = 
        f.length>0 && f(0)=='[' && f(f.length-1)==']'

    def apply(f: String): FeatureType = {
        val feaType = f.substring(1, f.length-1)
        feaType match {
        case "uid" => Uid
        case "oid" => Oid
        case "source" => Source
        case "length_title" => LengthTitle
        case "length_desc" => LengthDesc
        case "length_content" => LengthContent
        case _ => Unknown
        }
    }
}
