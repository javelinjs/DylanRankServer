package com.awkin.dylanrank

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter

import scala.actors._
import Actor._
import scala.math.exp

class Model extends Actor {
    import FeatureType._
    val logger = LoggerFactory.getLogger(classOf[Model])
    private val beta0Def = 0.0

    def act() {
        val (model, beta0) = loadModelFromFile
        calculator(model, beta0)
    }

    private def calculator(model: Map[FeatureType, Double], beta0: Double) {
        val (modelNew, beta0New) = 
        receive {
        case ("calculate", caller: Actor, id: Int, item: Map[FeatureType, Double]) =>
            logger.debug("calculate weight for item {}", item.toString)

            val beta = 
            (beta0 /: item.keys) { (b, fea) =>
                b + item(fea) * model(fea)    
            }
            logger.debug("beta for itemid {} = {}", id, beta)

            //result 
            val weight = 1 - 1 / (1 + exp(beta))
            logger.debug("weight for itemid {} = {}", id, weight)
            caller ! (id, weight)

            (model, beta0)
        case ("reload_from_file", caller: Actor) =>
            logger.info("reload model from file")
            loadModelFromFile
        }
        calculator(modelNew, beta0New)
    }

    private def loadModelFromFile(): (Map[FeatureType, Double], Double) = {
        logger.info("load model from {}", Config.modelData)

        val (_, model: Map[FeatureType, Double], beta0: Double, _) = 
        try {
            val lines = scala.io.Source.fromFile(Config.modelData).getLines
            ((0, Map[FeatureType, Double](), beta0Def, Array[Double]()) /: lines) { 
            (idx_feaMap_beta0_vector, line) =>
                val (idx, feaMap, lrBeta0, vector) = idx_feaMap_beta0_vector
                line match {
                case BeVector() =>
                    //return the weight vector string
                    (idx, feaMap, lrBeta0, BeVector(line))
                case BeFeatureType() =>
                    (
                        idx + 1, 
                        feaMap + (BeFeatureType(line) -> vector(idx)), 
                        lrBeta0,
                        vector
                    )
                case BeBeta0() => 
                    (idx, feaMap, BeBeta0(line), vector)
                case _ =>
                    (idx, feaMap, lrBeta0, vector)
                }
            }
        } catch {
            case ex =>
                logger.warn(ex.getMessage)
                logger.warn("unable to read model from {}", Config.modelData)
                logger.warn("use default model")
                //TODO: default model
        }

        (model, beta0)
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

object BeBeta0 {
    def unapply(b: String): Boolean = b.length>0 && b(0)=='+'
    def apply(b: String): Double = b.substring(1, b.length).toDouble
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
