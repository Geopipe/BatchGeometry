package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import com.geopipe.profiling.TicToc.{tic,toc}

class UniqueEffectsRewriter(collada:Node) extends PipelineRuleStage[Nothing] {
	
	class ReplaceEffectIdRewriter(replacements:Map[String, String]) extends RewriteRule {
		val updateEffect = new ElemNeedsUpdate("effect",Map(),new MetaDataNeedsUpdate(Set("name","id"), replacements))
		val updateNewParam = new ElemNeedsUpdate("newparam",Map(),new MetaDataNeedsUpdate(Set("sid"),replacements))
		val updateTexture = new ElemNeedsUpdate("texture",Map(),new MetaDataNeedsUpdate(Set("texture"),replacements))
		val updateSource = new ElemNeedsUpdate("source",replacements)
		
		override def transform(n: Node): Seq[Node] = {
			n match {
				case updateEffect(_,e) => e
				case updateNewParam(_,e) => e
				case updateTexture(_,e) => e
				case updateSource(_,e) => e
				case e:Elem => e
				case _ => n
			}
		}
	}
	
	tic
	val effects = collada \ "library_effects" \ "effect"
	val (uniqueEffects, replaceWith) = effects.foldLeft((Set[(String,Node)](),Map[String,String]())){
		case ((uniqueEffects, replaceWith), nodeHere) =>
			val effectId = nodeHere \@ "id"
			uniqueEffects.find{case (testId, testNode) =>
				val tryReplaceWith = Map(effectId -> testId)
				val replacer = new RuleTransformer(new ReplaceEffectIdRewriter(tryReplaceWith))
				val replacement = replacer(nodeHere)
				Utility.trim(testNode) xml_== Utility.trim(replacement) 
			}.fold( (uniqueEffects + ((effectId, nodeHere)), replaceWith) ){
				case (replaceId, _) => (uniqueEffects, replaceWith + (effectId -> replaceId)) 	
			}
	}
	toc("effect replacements")
	
	tic
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
	toc("material replacements")
	
	tic
	val triangles = collada \ "library_geometries" \ "geometry" \ "mesh" \ "triangles"
	val triangleNeedsUpdate = new ElemNeedsUpdate("triangles",Map(),new MetaDataNeedsUpdate(Set("material"),materialReplacements))
	val updatedTriangles = triangleNeedsUpdate.collectUpdates(triangles)
	toc("triangle replacements")
	
	tic
	val instanceMaterials = MiscHelpers.retrieveSceneNodes(collada) \ "instance_geometry" \ "bind_material" \\ "instance_material"
	val instanceMaterialNeedsUpdate = new ElemNeedsUpdate("instance_material",Map(),new MetaDataNeedsUpdate(Set("symbol","target"),materialReplacements))
	val updatedInstanceMaterials = instanceMaterialNeedsUpdate.collectUpdates(instanceMaterials)
	toc("instanceMat replacements")
	
	override def sideChannel() = Map()
	override def transform(n: Node): Seq[Node] = n match {
		case e:Elem =>
			e.label match {
				case "library_effects" => e.copy(child = uniqueEffects.map(_._2).toSeq)
				case "library_materials" => e.copy(child = updatedMaterials.map(_._2).toSeq)
				case "triangles" => updatedTriangles.getOrElse(e,e)
				case "instance_material" => updatedInstanceMaterials.getOrElse(e,e)
				case _ => e
			}
		case _ => n
	}
}