package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._


class UniqueImagesRewriter(collada:Node) extends RewriteRule {
	val images = collada \ "library_images" \ "image"
	val (uniqueTextures, replaceWith) = images.foldLeft((Map[String,(String,Node)](),Map[String,String]())){
		case ((uniqueTextures, replaceWith), nodeHere) =>
			val texPath = (nodeHere \ "init_from").text
			val imgId = nodeHere \@ "id"
			Console.println(s"This node (#$imgId) is pointing at $texPath")
			uniqueTextures.get(texPath) match {
				case None => 
					Console.println("\tthat's super unique, we'll keep it")
					(uniqueTextures + (texPath -> (imgId, nodeHere)),  replaceWith)
				case Some((replaceId,_)) => 
					Console.println(s"\tWe'll replace it with $replaceId")
					(uniqueTextures, replaceWith + (imgId -> replaceId))
			}
	}
	
	Console.println(uniqueTextures)
	Console.println(replaceWith)
	
	val effects = collada \ "library_effects" \ "effect"
	val effectNeedsUpdate = new EffectNeedsUpdate(replaceWith)
	
	val updatedEffects = effects.collect{ case effectNeedsUpdate(effect, update) => (effect -> update) }.toMap
		
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

class ReplaceEffectIdRewriter(replacements:Map[String, String]) extends RewriteRule {
	val updateEffect = new ElemNeedsUpdate("effect",Map(),new MetaDataNeedsUpdate(Set("name","id"), replacements))
	val updateNewParam = new ElemNeedsUpdate("newparam",Map(),new MetaDataNeedsUpdate(Set("sid"),replacements))
	val updateTexture = new ElemNeedsUpdate("texture",Map(),new MetaDataNeedsUpdate(Set("texture"),replacements))
	val updateSource = new ElemNeedsUpdate("source",replacements)
	
	
	override def transform(n: Node): Seq[Node] = {
		val ret = n match {
			case updateEffect(_,e) => e//.copy(child = e.child.map(this))
			case updateNewParam(_,e) => e//.copy(child = e.child.map(this))
			case updateTexture(_,e) => e
			case updateSource(_,e) => e
			case e:Elem => e
			case _ => n
		}
		/*
		if(ret == n){
			Console.println(s"\t\tDidn't update: \n$n")
		} else {
			Console.println(s"\t\tReplacing: \n$n with \n$ret")
		}
		*/
		ret
	}
	
}

class UniqueEffectsRewriter(collada:Node) extends RewriteRule {
	val effects = collada \ "library_effects" \ "effect"
	val (uniqueEffects, replaceWith) = effects.foldLeft((Set[(String,Node)](),Map[String,String]())){
		case ((uniqueEffects, replaceWith), nodeHere) =>
			val effectId = nodeHere \@ "id"
			Console.println(s"This effect (#$effectId) has ${nodeHere.child.length} children")
			uniqueEffects.find{case (testId, testNode) =>
				val tryReplaceWith = Map(effectId -> testId)
				val replacer = new RuleTransformer(new ReplaceEffectIdRewriter(tryReplaceWith))
				val replacement = replacer(nodeHere)
				Console.println(s"\tComparing <${replacement.label} ${replacement.attributes}> (new) vs <${testNode.label} ${testNode.attributes}> (old)") 
				//Console.println(s"\t$replacement\n\t$testNode") 
				Utility.trim(testNode) xml_== Utility.trim(replacement) 
			} match {
				case Some((replaceId, _)) => 
					Console.println(s"\tWe'll replace it with $replaceId")
					(uniqueEffects, replaceWith + (effectId -> replaceId)) 
				case None =>
					Console.println("\tIt's super unique, we'll keep it")
					(uniqueEffects + ((effectId, nodeHere)), replaceWith)
					
			}
	}
	
	/*
	val material_instance_effects =  collada \ "library_materials" \ "material" \ "instance_effect"
	val updateInstanceEffect = new ElemNeedsUpdate("instance_effect",Map(),new MetaDataNeedsUpdate(Set("url"),replaceWith))
	val instanceEffectUpdates = material_instance_effects.collect{	case updateInstanceEffect(ie, update) => (ie -> update)	}.toMap
	*/
	
	val materials = collada \ "library_materials" \ "material"
	val (updatedMaterials, materialReplacements) = materials.foldLeft((Set[(String,Elem)](), Map[String,String]())){
		case ((uniqueSoFar, replacementsSoFar), materialHere:Elem) =>
			val idHere = materialHere \@ "id"
			val materialDuplicatesEffect = new MaterialDuplicatesInstanceEffect(uniqueSoFar, replaceWith)
			materialHere match {
				case materialDuplicatesEffect(dupId) => 
					(uniqueSoFar , replacementsSoFar + (idHere -> dupId))
				case _ =>
					(uniqueSoFar + ((idHere, materialHere)), replacementsSoFar)
			}
	}
	
	val triangles = collada \ "library_geometries" \ "geometry" \ "mesh" \ "triangles"
	val triangleNeedsUpdate = new ElemNeedsUpdate("triangles",Map(),new MetaDataNeedsUpdate(Set("material"),materialReplacements))
	val updatedTriangles = triangles.collect {
		case triangleNeedsUpdate(triangle, update) =>  (triangle -> update)
	}.toMap
	
	override def transform(n: Node): Seq[Node] = n match {
		case e:Elem =>
			e.label match {
				case "library_effects" => e.copy(child = uniqueEffects.map(_._2).toSeq)
				case "library_materials" => e.copy(child = updatedMaterials.map(_._2).toSeq)
				case "triangles" => updatedTriangles.getOrElse(e,e)
				case _ => e
			}
		case _ => n
	}
}

object BatchGeometry {
	def makeImagesUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueImagesRewriter(collada))
		transformer(collada)
	}
	
	def makeEffectsUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueEffectsRewriter(collada))
		transformer(collada)
	}
	
	def main(args:Array[String]):Unit = {
		val collada = XML.loadFile(args(0))
		val withUniqueImages = makeImagesUnique(collada)
		val withUniqueEffects = makeEffectsUnique(withUniqueImages)
		
		Console.println(collada \\ "library_images")
		Console.println(collada \\ "library_effects")
		Console.println()
		Console.println(withUniqueImages \\ "library_images")
		Console.println(withUniqueImages \\ "library_effects")
		Console.println(withUniqueEffects \\ "library_materials")
		Console.println()
		Console.println(withUniqueEffects \\ "library_effects")
		Console.println(withUniqueEffects \\ "library_materials")
		Console.println((withUniqueEffects \\ "triangles").collect{case t:Elem => s"<triangles ${t.attributes}"})
		
	}
}