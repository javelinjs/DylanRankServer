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
        calculator(model, beta0, Statistics(None, -9999999.0, 9999999.0))
    }

    private def calculator(model: Map[FeatureType, ModelFactor], 
                            beta0: Double, 
                            stat: Statistics) {

        val (modelNew, beta0New, statNew) = 
        receive {
        case ("calculate", caller: Actor, id: Int, item: Map[FeatureType, Double]) =>
            if (logger.isDebugEnabled)
                logger.debug("calculate rank for item {}", item.toString)

            val beta = 
            (beta0 /: model.keys) { (b, fea) =>
                b + item(fea)/model(fea).normalizer * model(fea).weight
            }
            if (logger.isDebugEnabled)
                logger.debug("beta for itemid {} = {}", id, beta)

            //result 
            val rank = 1 - 1 / (1 + exp(beta))
            val statisticNew = Statistics(Some(stat), rank, rank)
            if (logger.isDebugEnabled) {
                logger.debug("rank for itemid {} = {}", id, rank)
                logger.debug("new stat: {}", statisticNew.toString)
            }

            caller ! (id, rank, statisticNew)

            (model, beta0, statisticNew)
        case ("reload_from_file", caller: Actor) =>
            logger.info("reload model from file")
            val (modelN, beta0N) = loadModelFromFile
            (modelN, beta0N, stat)
        case _ =>
            logger.warn("Unknown request")
            (model, beta0, stat)
        }
        calculator(modelNew, beta0New, statNew)
    }

    private def loadModelFromFile(): 
    (Map[FeatureType, ModelFactor], Double) = {
        logger.info("load model from {}", Config.modelData)

        val (_, model:Map[FeatureType, ModelFactor], beta0:Double, _, _) = 
        try {
            val lines = scala.io.Source.fromFile(Config.modelData).getLines
            ((0, Map[FeatureType, ModelFactor](), beta0Def, 
                Array[Double](), Array[Double]()) /: lines) {
            (idx_feaMap_beta0_vector_normalizer, line) =>
                val (idx, feaMap, lrBeta0, vector, feaNormalizer) = 
                    idx_feaMap_beta0_vector_normalizer
                line match {
                case BeVector() =>
                    //return the weight vector string
                    (idx, feaMap, lrBeta0, BeVector(line), feaNormalizer)
                case BeNormalizer() =>
                    //return the normalizer vector string
                    (idx, feaMap, lrBeta0, vector, BeNormalizer(line))
                case BeFeatureType() =>
                    //extract weight and normalizer
                    val modelFactor = 
                        ModelFactor(vector(idx), feaNormalizer(idx))
                    (
                        idx + 1, 
                        feaMap + (BeFeatureType(line) -> modelFactor), 
                        lrBeta0,
                        vector,
                        feaNormalizer
                    )
                case BeBeta0() => 
                    (idx, feaMap, BeBeta0(line), vector, feaNormalizer)
                case _ =>
                    (idx, feaMap, lrBeta0, vector, feaNormalizer)
                }
            }
        } catch {
            case ex =>
                logger.warn(ex.getMessage)
                logger.warn("unable to read model from {}", Config.modelData)
                logger.warn("use default model")
                //TODO: default model
        }

        logger.info("model: {}, beta0: {}", model.toString, beta0)

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

object BeNormalizer {
    def unapply(v: String): Boolean = 
        v.length>0 && v(0)=='{' && v(v.length-1)=='}'

    def apply(v: String): Array[Double] = {
        val normArray = v.substring(1, v.length-1).split(",")
        (List[Double]() /: normArray) { (normList, sValue) =>
            normList ++ List[Double](sValue.toDouble)
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

class Statistics(val rankMax: Double, val rankMin: Double) { 
    override def toString = {
        "rankMax: %f, rankMin: %f".format(rankMax, rankMin)
    }
}
object Statistics {
    def apply(oldstat: Option[Statistics], 
                rankMax: Double, rankMin: Double) = {
        oldstat match {
        case None => 
            new Statistics(rankMax, rankMin)
        case someStat =>
            val stat = someStat.get
            val rankMaxNew = 
                if (rankMax > stat.rankMax) rankMax
                else stat.rankMax
            val rankMinNew = 
                if (rankMin < stat.rankMin) rankMin 
                else stat.rankMin

            new Statistics(rankMaxNew, rankMinNew)
        }
    }
}

class ModelFactor(val weight: Double, val normalizer: Double) {
    override def toString = {
        "weight: %f, normalizer: %f".format(weight, normalizer)
    }
}
object ModelFactor {
    def apply(weight: Double, normalizer: Double) = {
        new ModelFactor(weight, normalizer)
    }
}
