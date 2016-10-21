package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._
import scala.language.implicitConversions

object MetaDataHelpers {
	import scala.collection.{LinearSeq, LinearSeqOptimized}
	import scala.collection.mutable.{Builder, ListBuffer}

	class MetaDataEnhanced(val metadata:MetaData) extends Iterable[MetaDataEnhanced] {
		override def iterator = metadata.iterator.map(enhanceMetaData(_))

				val maybePre:Option[String] = metadata match {
				case a:Attribute => Option(a.pre)
				case Null => None
		}

		def key = metadata.key
				def value = metadata.value

				def copyEnhanced(maybePre:Option[String] = this.maybePre, key:String = this.key, value:Either[String,Seq[Node]] = Right(this.value)):MetaDataEnhanced = {
			metadata match {
			case Null => throw new IllegalStateException("Can't copy Null metadata with new pre:key=value")
			case _ => Attribute(maybePre, key, value.left.map(Text(_).theSeq).merge, metadata.next)
			}
		}
	}

	implicit def enhanceMetaData(md:MetaData): MetaDataEnhanced = new MetaDataEnhanced(md)
	implicit def iterableToMetaData(items: Iterable[MetaData]): MetaData = {
		items match {
		case Nil => Null
		case head :: tail => head.copy(next=iterableToMetaData(tail))
		}
	}
}

object ReplacementHelpers {
	def replaceAll(initText:String, replacements:Map[String,String]):Option[String] = {
			val newText = replacements.toSeq.sortBy( - _._1.length).foldLeft(initText){
				(srcText,kvPair) => srcText.replace(kvPair._1, kvPair._2)
			}
			if(newText == initText){ None } else { Some(newText) }
	}

	def mapWhereDefined(children:Seq[Node],f:Node => Option[Node]):Seq[Node] = {
		children.map{ child => f(child).getOrElse(child) }
	}
}

class MetaDataNeedsUpdate(targetAttrs:Function1[String,Boolean], replacements:Map[String, String]) {
	def unapply(oldMetaData:MetaData):Option[MetaData] = {
		import MetaDataHelpers.{enhanceMetaData, iterableToMetaData}
		val (hasUpdatedMetaData, newMetaData) = oldMetaData.foldLeft((false,List[MetaData]())){
			case ((needsUpdate, metaSoFar), metaHere)=> 
				((needsUpdate, targetAttrs(metaHere.key)) match {
					case (_, true) =>
						val oldText = metaHere.value.text
						val maybeNewText = ReplacementHelpers.replaceAll(oldText, replacements)
						maybeNewText.map{
							newText => (true, metaSoFar :+ metaHere.copyEnhanced(value=Left(newText)).metadata)
						}
							
					case _ => None
				}).getOrElse((needsUpdate, metaSoFar :+ metaHere))
		}
		if(hasUpdatedMetaData){
			//Console.println(s"MD update, $oldMetaData -> ${iterableToMetaData(newMetaData)} in $targetAttrs by way of $replacements")
			Some(newMetaData)
		} else {
			None
		}
	}
}

class ElemNeedsUpdate(label:String, textReplacements:Map[String, String], newMd:MetaDataNeedsUpdate = new MetaDataNeedsUpdate({s:String => false}, Map())) {
	def unapply(e:Elem):Option[(Elem,Elem)] = e.label match {
		case `label` => 
			val oldText = e.text
			val oldAttrs = e.attributes
			(oldAttrs, ReplacementHelpers.replaceAll(oldText, textReplacements)) match {
				case (newMd(nAttr), maybeNewText) =>
					val copied = e.copy(child = maybeNewText.map(newText => Text(newText).theSeq).getOrElse(e.child), attributes = nAttr) 
					Console.println(s"\t\tMatch! Return new attributes and maybe new text: <${e.label} ${e.attributes}> -> <${copied.label} ${copied.attributes}>")
					Some(e -> copied)
				case (_, Some(newText)) => 
					val copied = e.copy(child = Text(newText).theSeq)
					Console.println(s"\t\tMatch! Return new text: $e -> $copied")
					Some(e -> copied)
				case _ => None
			}
		case _ => None
	}
}

class SurfaceNeedsUpdate(replacements:Map[String,String]) {
	def unapply(surface:Elem):Option[(Elem, Elem)] = {
		(surface \ "init_from").headOption match {
			case Some(init:Elem) =>							
				val oldText = init.text
				val maybeNewText = replacements.get(oldText)
				maybeNewText.map(newText => surface -> surface.copy(child = init.copy(child = Text(newText))))
			case _ => None
		}
	}
}

class SamplerNeedsUpdate(replacements:Map[String,String]) {
	def unapply(sampler:Elem):Option[(Elem, Elem)] = {
		(sampler \ "source").headOption match {
			case Some(src:Elem) =>
				val oldText = src.text
				val maybeNewText = ReplacementHelpers.replaceAll(oldText, replacements)
				maybeNewText.map(newText => sampler -> sampler.copy(child = src.copy(child = Text(newText))))
			case _ => None
		}
	}
}

class TextureNeedsUpdate(replacements:Map[String,String]) {
	val metaDataNeedsUpdate = new MetaDataNeedsUpdate(Set("texture"), replacements)
	def unapply(texture:Elem):Option[(Elem, Elem)] = {
		val oldMetaData = texture.attributes
		val maybeNewMetaData = metaDataNeedsUpdate.unapply(oldMetaData)
		maybeNewMetaData.map{ newMetaData => (texture -> texture.copy(attributes = newMetaData)) }
	}
}

class ParamNeedsUpdate(updatedSurfaces:Map[Elem,Elem], updatedSamplers:Map[Elem,Elem], replacements:Map[String,String]) {
	val metaDataNeedsUpdate = new MetaDataNeedsUpdate(Set("sid"), replacements)
	def unapply(newParam:Elem):Option[(Elem,Elem)] = {
		val surfaces = newParam \ "surface"
		val samplers = newParam \ "sampler2D"
		val hasUpdatedSurfaces = surfaces.collect{ case surface:Elem => updatedSurfaces.contains(surface)}.exists(identity)
		val hasUpdatedSamplers = samplers.collect{ case sampler:Elem => updatedSamplers.contains(sampler)}.exists(identity)
		
		val metaData = newParam.attributes
		val maybeUpdatedMetaData = metaDataNeedsUpdate.unapply(metaData)
			
		if(maybeUpdatedMetaData.isDefined || hasUpdatedSurfaces || hasUpdatedSamplers){
			Some(newParam -> newParam.copy(attributes = maybeUpdatedMetaData.getOrElse(metaData), child = ReplacementHelpers.mapWhereDefined(newParam.child, {
					case e:Elem => updatedSurfaces.get(e).orElse(updatedSamplers.get(e))
					case n:Node => None
			})))
		} else {
			None
		}
	}
}

class MaterialDuplicatesInstanceEffect(uniques:Set[(String, Elem)], replacements:Map[String,String]) {
	def unapply(material:Elem):Option[String] = {
		(material \ "instance_effect").headOption.flatMap{
			case ie:Elem =>
				uniques.collectFirst{
					case (testId, testMat) if
						(testMat \ "instance_effect").headOption.map{
						case testIe:Elem =>
							val testUrl = (testIe \@ "url") 
							val urlHere = (ie \@ "url")
							(testUrl == urlHere) || ((urlHere.splitAt(1), testUrl.splitAt(1)) match {
								case (("#",ieIdHere), ("#",testIeId)) if replacements.get(ieIdHere) == Some(testIeId) => true
								case _ => false
							})
						}.getOrElse(false) =>
							testId
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

class EffectNeedsUpdate(replacements:Map[String,String]) {
	val surfaceNeedsUpdate = new SurfaceNeedsUpdate(replacements)
	val samplerNeedsUpdate = new SamplerNeedsUpdate(replacements)
	val textureNeedsUpdate = new TextureNeedsUpdate(replacements)
	
	def unapply(effect:Elem):Option[(Elem, Elem)] = {
		(effect \ "profile_COMMON").headOption match {
			case Some(profile:Elem) =>
				val effectProfile = effect \ "profile_COMMON"
				val newParams = effectProfile \ "newparam"
				val textures = effectProfile \ "technique" \\ "texture"
				
				val updatedSurfaces = (newParams \ "surface").collect{ case surfaceNeedsUpdate(surface, update) => (surface -> update)}.toMap
				val updatedSamplers = (newParams \ "sampler2D").collect{ case samplerNeedsUpdate(sampler, update) => (sampler -> update)}.toMap
				
				val updatedTextures = textures.collect{ case textureNeedsUpdate(texture, update) => (texture -> update)}.toMap
				
				val techniqueTransformer = new RuleTransformer(new TechniqueRewriter(updatedTextures))
				val updatedTechniques = (effectProfile \ "technique").map(t => (t -> techniqueTransformer(t))).toMap

				val paramUpdater = new ParamNeedsUpdate(updatedSurfaces, updatedSamplers, replacements)

				val updatedNewParams = newParams.collect{ case paramUpdater(newParam, update) => (newParam -> update)}.toMap
				
				Some(effect -> effect.copy(child = profile.copy( child = ReplacementHelpers.mapWhereDefined(profile.child, {
					case e:Elem => updatedNewParams.get(e).orElse(updatedTechniques.get(e))
					case n:Node => None
				}))))
			case _ => None
			
		}
	}
}

