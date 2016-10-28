package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

class UniqueImagesRewriter(collada:Node) extends RewriteRule {
	val images = collada \ "library_images" \ "image"
	val (uniqueTextures, replaceWith) = images.foldLeft((Map[String,(String,Node)](),Map[String,String]())){
		case ((uniqueTextures, replaceWith), nodeHere) =>
			val texPath = (nodeHere \ "init_from").text
			val imgId = nodeHere \@ "id"
			uniqueTextures.get(texPath).fold((uniqueTextures + (texPath -> (imgId, nodeHere)),  replaceWith)){
				case (replaceId,_) => (uniqueTextures, replaceWith + (imgId -> replaceId))
			}
	}
	
	val effects = collada \ "library_effects" \ "effect"
	val effectNeedsUpdate = new EffectNeedsUpdate(replaceWith)
	val updatedEffects = effectNeedsUpdate.collectUpdates(effects)
		
	override def transform(n: Node): Seq[Node] = { 
		n match {
			case e:Elem =>
				e.label match {
					case "effect" => updatedEffects.getOrElse(e, e)
					case "library_images" => e.copy(child = uniqueTextures.map(_._2._2).toSeq)
					case _ => e
				}
			case  _ => n
		}
	}
}