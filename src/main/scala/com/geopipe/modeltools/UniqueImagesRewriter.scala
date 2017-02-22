package com.geopipe.modeltools

import scala.xml._

import com.geopipe.profiling.TicToc.{tic,toc}

class UniqueImagesRewriter(collada:Node) extends PipelineRuleStage[Nothing] {
	tic
	val images = collada \ "library_images" \ "image"
	val (uniqueTextures, replaceWith) = images.foldLeft((Map[String,(String,Node)](),Map[String,String]())){
		case ((uniqueTextures, replaceWith), nodeHere) =>
			val texPath = (nodeHere \ "init_from").text
			val imgId = nodeHere \@ "id"
			uniqueTextures.get(texPath).fold((uniqueTextures + (texPath -> (imgId, nodeHere)),  replaceWith)){
				case (replaceId,_) => (uniqueTextures, replaceWith + (imgId -> replaceId))
			}
	}
	toc("uniqueTextureMap")
	
	tic
	val effects = collada \ "library_effects" \ "effect"
	val effectNeedsUpdate = new EffectNeedsUpdate(replaceWith)
	val updatedEffects = effectNeedsUpdate.collectUpdates(effects)
	toc("updateEffectsMap")
	
	private val libraryImpl:PartialFunction[Node, Elem] = MiscHelpers.elemFiltIf(_.label == "library_images"){
		case e => e.copy(child = uniqueTextures.map(_._2._2).toSeq)
	}
	private val effectImpl = MiscHelpers.elemFilt(updatedEffects)
	override protected val impl = effectImpl.orElse(libraryImpl)
}