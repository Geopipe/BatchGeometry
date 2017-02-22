package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform.RewriteRule

import com.geopipe.xml.RuleApplicator

trait ElemMatchAndUpdate {
	private[this] val self = this // Apparently can't use this as an extractor directly???
	def unapply(e:Elem):Option[(Elem,Elem)]
	def collectUpdates(n:Seq[Node]):Map[Elem,Elem] = n.collect{ case self(e,u) => (e -> u) }.toMap
}

class MetaDataNeedsUpdate(targetAttrs:Function1[String,Boolean], replacements:Map[String, String]) {
	import MiscHelpers._
	def unapply(oldMetaData:MetaData):Option[MetaData] = {
		import MetaDataHelpers.{enhanceMetaData, iterableToMetaData}
		val (hasUpdatedMetaData, newMetaData) = oldMetaData.foldLeft((false,List[MetaData]())){
			case ((needsUpdate, metaSoFar), metaHere)=> 
				targetAttrs(metaHere.key).withOption {
						val oldText = metaHere.value.text
						val maybeNewText = ReplacementHelpers.replaceAll(oldText, replacements)
						maybeNewText.map{ newText => (true, metaSoFar :+ metaHere.copyEnhanced(value=Left(newText)).metadata) }		
				}.getOrElse((needsUpdate, metaSoFar :+ metaHere))
		}
		hasUpdatedMetaData.toOption(newMetaData)
	}
}

class ElemNeedsUpdate(label:String, 
		textReplacements:Map[String, String], 
		newMd:MetaDataNeedsUpdate = new MetaDataNeedsUpdate({s:String => false}, Map())) extends ElemMatchAndUpdate {
	import MiscHelpers._
	override def unapply(e:Elem):Option[(Elem,Elem)] =
		(e.label == label).withOption {
			val oldText = e.text
			val oldAttrs = e.attributes
			(oldAttrs, ReplacementHelpers.replaceAll(oldText, textReplacements)) match {
				case (newMd(nAttr), maybeNewText) => Some(e -> e.copy(child = maybeNewText.map(newText => Text(newText).theSeq).getOrElse(e.child), attributes = nAttr))
				case (_, Some(newText)) => Some(e -> e.copy(child = Text(newText).theSeq))
				case _ => None
			}
		}
			
}

class SurfaceNeedsUpdate(replacements:Map[String,String]) extends ElemMatchAndUpdate {
	override def unapply(surface:Elem):Option[(Elem, Elem)] = 
		(surface \ "init_from").collectFirst {
			case init:Elem =>							
				val oldText = init.text
				val maybeNewText = replacements.get(oldText)
				maybeNewText.map(newText => surface -> surface.copy(child = init.copy(child = Text(newText))))
		}.flatten
}

class SamplerNeedsUpdate(replacements:Map[String,String]) extends ElemMatchAndUpdate {
	override def unapply(sampler:Elem):Option[(Elem, Elem)] =
		(sampler \ "source").collectFirst {
			case src:Elem =>
				val oldText = src.text
				val maybeNewText = ReplacementHelpers.replaceAll(oldText, replacements)
				maybeNewText.map(newText => sampler -> sampler.copy(child = src.copy(child = Text(newText))))
		}.flatten
}

class TextureNeedsUpdate(replacements:Map[String,String]) extends ElemMatchAndUpdate {
	val metaDataNeedsUpdate = new MetaDataNeedsUpdate(Set("texture"), replacements)
	override def unapply(texture:Elem):Option[(Elem, Elem)] = {
		val oldMetaData = texture.attributes
		val maybeNewMetaData = metaDataNeedsUpdate.unapply(oldMetaData)
		maybeNewMetaData.map{ newMetaData => (texture -> texture.copy(attributes = newMetaData)) }
	}
}

class ParamNeedsUpdate(updatedSurfaces:Map[Elem,Elem], updatedSamplers:Map[Elem,Elem], replacements:Map[String,String]) extends ElemMatchAndUpdate {
	import MiscHelpers._
	
	val metaDataNeedsUpdate = new MetaDataNeedsUpdate(Set("sid"), replacements)
	override def unapply(newParam:Elem):Option[(Elem,Elem)] = {
		val surfaces = newParam \ "surface"
		val samplers = newParam \ "sampler2D"
		val hasUpdatedSurfaces = surfaces.collect{ case surface:Elem => updatedSurfaces.contains(surface)}.exists(identity)
		val hasUpdatedSamplers = samplers.collect{ case sampler:Elem => updatedSamplers.contains(sampler)}.exists(identity)
		
		val metaData = newParam.attributes
		val maybeUpdatedMetaData = metaDataNeedsUpdate.unapply(metaData)
			
		(maybeUpdatedMetaData.isDefined || hasUpdatedSurfaces || hasUpdatedSamplers).toOption{
			newParam -> newParam.copy(attributes = maybeUpdatedMetaData.getOrElse(metaData), child = ReplacementHelpers.mapWhereDefined(newParam.child, {
				case e:Elem => updatedSurfaces.get(e).orElse(updatedSamplers.get(e))
				case n:Node => None
			}))
		}
	}
}

class MaterialDuplicatesInstanceEffect(uniques:Set[(String, Elem)], replacements:Map[String,String]) {
	def unapply(material:Elem):Option[String] = {
		(material \ "instance_effect").headOption.flatMap{
			case ie:Elem =>
				uniques.collectFirst{
					case (testId, testMat) if
						(testMat \ "instance_effect").collectFirst {
						case testIe:Elem =>
							val testUrl = (testIe \@ "url") 
							val urlHere = (ie \@ "url")
							(testUrl == urlHere) || ((urlHere.splitAt(1), testUrl.splitAt(1)) match {
								case (("#",ieIdHere), ("#",testIeId)) if replacements.get(ieIdHere) == Some(testIeId) => true
								case _ => false
							})
						}.getOrElse(false) => testId
				}
		}
	}
}

class TechniqueRewriter(updatedTextures:Map[Elem, Elem]) extends RewriteRule {
	override def transform(n: Node): Seq[Node] = {
		n match {
			case e:Elem if e.label == "texture" =>	updatedTextures.getOrElse(e, e)
			case _ => n
		}
	}
}

class EffectNeedsUpdate(replacements:Map[String,String]) extends ElemMatchAndUpdate {
	val surfaceNeedsUpdate = new SurfaceNeedsUpdate(replacements)
	val samplerNeedsUpdate = new SamplerNeedsUpdate(replacements)
	val textureNeedsUpdate = new TextureNeedsUpdate(replacements)
	
	override def unapply(effect:Elem):Option[(Elem, Elem)] = {
		(effect \ "profile_COMMON").collectFirst {
			case profile:Elem =>
				val effectProfile = effect \ "profile_COMMON"
				val newParams = effectProfile \ "newparam"
				val textures = effectProfile \ "technique" \\ "texture"
				
				val updatedSurfaces = surfaceNeedsUpdate.collectUpdates(newParams \ "surface")
				val updatedSamplers = samplerNeedsUpdate.collectUpdates(newParams \ "sampler2D")
				
				val updatedTextures = textureNeedsUpdate.collectUpdates(textures)
				
				val techniqueTransformer = new RuleApplicator(new TechniqueRewriter(updatedTextures))
				val updatedTechniques = (effectProfile \ "technique").map(t => (t -> techniqueTransformer(t))).toMap

				val paramUpdater = new ParamNeedsUpdate(updatedSurfaces, updatedSamplers, replacements)
				val updatedNewParams = paramUpdater.collectUpdates(newParams)
				
				(effect -> effect.copy(child = profile.copy( child = ReplacementHelpers.mapWhereDefined(profile.child, {
					case e:Elem => updatedNewParams.get(e).orElse(updatedTechniques.get(e))
					case n:Node => None
				}))))
		}
	}
}

