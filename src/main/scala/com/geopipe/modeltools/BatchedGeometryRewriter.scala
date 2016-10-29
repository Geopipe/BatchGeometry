package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

class BatchedGeometryRewriter(collada:Node) extends PipelineRuleStage[JValue] {
	private val indexedSceneNodes = MiscHelpers.retrieveSceneNodes(collada).zipWithIndex
	private val geometryById = (collada \ "library_geometries" \ "geometry").map{
		case e:Elem => (e\@"id" -> e)
	}.toMap
	private val metaData = JObject(indexedSceneNodes.map{
		case (node, i) => 
			val metaDataRoot = node \ "extra" \ "technique"
			(i.toString, JObject(metaDataRoot.collectFirst{case e:Elem if e\@"profile" == "geopipe_Metadata" => e}.map{
				e => (e \ "param").collect{
					case p:Elem => ((p\@"semantic"), JString(p.text))
				}
			}.getOrElse(Nil).toList))
	}.toList)
	
	private val batchBuckets = indexedSceneNodes.flatMap{
		case(node:Elem, i) if Set("0 0 0","").contains((node\"translate").text) => 
			(node\"instance_geometry").collect{case e:Elem =>(e,i)}
		case (n, _) => throw new UnsupportedOperationException(s"The current version of this tool does not handle the case where the node base-point is not at the origin, but we got '${n\"translate"}'")
	}.groupBy{
		case (instance_geometry, i) => (instance_geometry \ "bind_material" \ "technique_common" \ "instance_material" \@ "target")
	}
	private def floatListForStride(vertAttrs:Map[Int,NodeSeq], stride:Int):List[Float] = {
		vertAttrs.get(stride).map{attr => (attr\"float_array").text.trim.split(" ").map(_.toFloat).toList}.getOrElse(Nil)
	}
	
	private val batchedArrays = batchBuckets.map{
		case(matUrl, geomToBatch) => 
			(matUrl, geomToBatch.foldLeft((List[Float](),List[Float](),List[Float](),List[Int]())){
				case((batchIDs,positions, texCoords, indices),(instance_geometry,batchId)) =>
					(instance_geometry \@ "url").splitAt(1) match {
						case ("#",geomID) => 
							val (pHere, tCHere, iHere) = geometryById.get(geomID).flatMap{geom => (geom \ "mesh").headOption}.collectFirst{
								case mesh:Elem =>
									val vertAttrs = (mesh\"source").groupBy{e => (e\"technique_common"\"accessor"\@"stride").toInt}
									(	floatListForStride(vertAttrs,3), floatListForStride(vertAttrs,2),
										(mesh\"triangles"\"p").text.trim.split(" ").map(_.toInt))
							}.getOrElse{throw new RuntimeException(s"Can't find a mesh element for geometry with id '$geomID'")}
							
							val nPVerts = pHere.length / 3
							val nTVerts = tCHere.length / 2
							assert((nPVerts == nTVerts) || (nTVerts == 0))
							
							val prevNVerts = positions.length / 3
							(batchIDs ++ List.fill(nPVerts)(batchId.toFloat), positions ++ pHere, texCoords ++ tCHere, indices ++ iHere.map{_ + prevNVerts})
						case _ => throw new UnsupportedOperationException("We don't support non-fragment url's for geometry")
					}
			})
	}
	
	private def joinId(components:String*):String = components.mkString("-")
	private def genFloatArray(contents:Seq[Float], strideNames:Seq[String], parentId:String, semantic:String):(String,Elem) = {
		val littleS = semantic.toLowerCase
		val id = joinId(parentId, littleS)
		val arrayId = joinId(id, "array")
		val strideLen = strideNames.length
		val xmlOut = <source id={id}>
			<float_array count={contents.length.toString} id={arrayId}>{contents.mkString(" ")}</float_array>
			<technique_common>
				<accessor stride={strideLen.toString} count={(contents.length / strideLen).toString} source={s"#$arrayId"}>
					{strideNames.map{name =>
							<param type="float" name={name}/>
					}}
				</accessor>
			</technique_common>
		</source>
		(id, xmlOut)
	}
	
	private val newGeoms = batchedArrays.zipWithIndex.map{
		case((matUrl, (batchIDs,positions, texCoords, indices)), batchI) =>
			val geomId = s"batch$batchI"
			val vertsId = joinId(geomId,"vertex")
			val attributes = Map(
					"POSITION" -> (positions, List("X","Y","Z")),
					"TEXCOORD" -> (texCoords, List("U","V")),
					"BATCHID" -> (batchIDs, List("I"))).collect{ case(sem,(con,sN)) if con.length > 0 => (sem->genFloatArray(con,sN,geomId,sem))}
			val vertCount = indices.length / 3
			val xmlOut = <geometry id={geomId}>
				<mesh>
					{attributes.map{ _._2._2 }}
					<vertices id={vertsId}>
						<input source={s"#${attributes("POSITION")._1}"} semantic="POSITION" />
					</vertices>
					<triangles material={matUrl.tail} count={vertCount.toString}>
						{attributes.zipWithIndex.map{
							case ((sem,(id,_)),i) =>
								val (tId, tSem) = sem match {
									case "POSITION" => (vertsId, "VERTEX")
									case _ => (id, sem)
								}
								<input offset={i.toString} source={s"#$tId"} semantic={tSem}/>
						}}
						<p>{indices.mkString(" ")}</p>
					</triangles>
				</mesh>
			</geometry>	
			(matUrl -> (attributes.contains("TEXCOORD"),xmlOut))
	}
	
	private val newNode = <node type="NODE">
		<translate>0 0 0</translate>
		{
		 newGeoms.map{
			 case(matUrl, (hasTexCoord, geometry)) =>
				<instance_geometry url={s"#${geometry\@"id"}"}>
					<bind_material>
						<technique_common>
							<instance_material target={matUrl} symbol={matUrl.tail}>
								{
									if(hasTexCoord){
										<bind_vertex_input input_semantic="TEXCOORD" semantic="UVSET0"/>
									}
								}
							</instance_material>
						</technique_common>
					</bind_material>
				</instance_geometry>
		 }
		}
	</node>
	
	override def sideChannel =	Map(classOf[BatchedGeometryRewriter] -> metaData)
	override def transform(n:Node):Seq[Node] = {
		n match {
			case e:Elem =>
				e.label match {
					case "library_geometries" => e.copy(child = newGeoms.map(_._2._2).toSeq)
					case "visual_scene" => e.copy(child = newNode)
					case _ => e
				}
			case _ => n
		}
	}
}