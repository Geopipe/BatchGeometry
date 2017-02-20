package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

class BatchedGeometryRewriter(collada:Node) extends PipelineRuleStage[JValue] {
	import MiscHelpers.{toIdDict, FragURL}
	
	
	private val indexedSceneNodes = MiscHelpers.retrieveSceneNodes(collada).zipWithIndex
	private val geometryById = toIdDict(collada \ "library_geometries" \ "geometry")
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
	
	private def getInput(e:Elem):((String, Option[Int]), String) = {
		e \@ "source" match {
			case FragURL(id) =>
				((e \@ "semantic", e.attribute("set").map(_.text.toInt)), id)
		}
	}
	private def getInput(ns:NodeSeq, sem:String):Option[(String, Elem)] = {
		ns.collectFirst{
			case e:Elem if (e \@ "semantic") == sem =>
				e \@ "source" match {
					case FragURL(id) =>
						(id, e)
				}
		}
	}
	
	private def srcAccessor(e:Elem):NodeSeq = {
		e\"technique_common"\"accessor"
	}
	
	/*************************************************
	 * Operating assumptions:
	 * - every mesh which uses the same material has
	 * - (a) the same set of semantics
	 * - (b) the same assignment of semantics to
	 * index offsets.
	 * - all params for the same source array have the same type (and are packed)
	 *************************************************/
	private val batchedArrays = batchBuckets.map{
		case(matUrl, geomToBatch) => 
			(matUrl, geomToBatch.foldLeft((List[Int](),Map[(String,Int,Option[Int]),List[_ <: AnyVal]](),Map[(String,Int,Option[Int]),(String,List[String])](),List[Int]())){
				case((batchIDs,semVertsMap, ofsSemMap, indices),(instance_geometry,batchId)) =>
					(instance_geometry \@ "url") match {
						case FragURL(geomID) =>
							val (sVMHere, oSMHere, iHere) = geometryById.get(geomID).flatMap{geom => (geom \ "mesh").headOption}.collectFirst{
								case mesh:Elem =>
									val byId = toIdDict(mesh \\ "_")
									val tris = (mesh \ "triangles")
									val triInputs = tris \ "input"
									val iHere = (tris \ "p").text.trim.split(" ").map(_.toInt)
									
									val oSMHere = triInputs.groupBy { case i:Elem => (i \@ "offset").toInt }.map {
										case (offset, semsAtOfs) => (offset -> semsAtOfs.map{
											case e:Elem => 
												val (sem, srcId) = getInput(e) match {
													case ((semN@"VERTEX", semS), vId) =>
														getInput(byId(vId) \ "input", "POSITION").map {
															case (pId, _) => ((semN, semS), pId)
														}.getOrElse(throw new RuntimeException("Position is required"))
													case r@_ => r
												}
												val params = srcAccessor(byId(srcId)) \ "param"
												(srcId -> (sem, params.head \@ "type", params.map{_ \@ "name"}.toList))
										}.toMap)
									}
									val sVMHere = oSMHere.flatMap{
										case (o, m) => m.map{
											case (id, (sem, vType, _)) =>
												val numericize:(String => AnyVal) = vType match {
													case "float" => {s:String => s.toFloat}
													case "int" => {s:String => s.toInt}
													case "boolean" => {s:String => s.toBoolean}
												}
												((sem._1, o, sem._2) -> (byId(id) \ s"${vType}_array").text.trim.split(" ").map(numericize).toList)
										}
											
									}
									
									(sVMHere, oSMHere, iHere)
							}.getOrElse{throw new RuntimeException(s"Can't find a mesh element for geometry with id '$geomID'")}
							
							val nBatchIndex = batchIDs.length
							val windowLen = oSMHere.size
							
							val ofsSoFar = Array.tabulate(windowLen){
								case ofs =>
									val sVMStatus = oSMHere(ofs).head._2
									val sVMKey = sVMStatus._1
									semVertsMap.get((sVMKey._1, ofs, sVMKey._2)).map{_.size}.getOrElse(0)
							}
							
							(batchIDs :+ batchId, sVMHere.map{
								case (s, l) =>
									(s -> semVertsMap.get(s).map{
										_ ++ l
									}.getOrElse(l))
							}, oSMHere.flatMap{
								case (o, m) => m.map {
									case (id, ((semN, semS), vType, vNs)) =>
										((semN, o, semS) -> (vType, vNs))
								}
							}, indices ++ iHere.grouped(windowLen).flatMap{
								_.zip(ofsSoFar).map{p => p._1 + p._2} // :+ nBatchIndex
							})
						case _ => throw new UnsupportedOperationException("We don't support non-fragment url's for geometry")
					}
			})
	}
	
	private def joinId(components:String*):String = components.mkString("-")
	private def genArrayOfType(contents:Seq[_ <: AnyVal], strideNames:Seq[String], parentId:String, semantic:String, offset:Int, set:Option[Int], vType:String):(String,Elem) = {
		val littleS = semantic.toLowerCase match {
			case "vertex" => "position"
			case s@_ => s
		}
		val id = joinId(List(parentId, littleS, offset.toString) ++ set.map{_.toString} :_*)
		val arrayId = joinId(id, "array")
		val strideLen = strideNames.length
		val xmlOut = <source id={id}>
			{
				val arrayFrag = <_array count={contents.length.toString} id={arrayId}>{contents.mkString(" ")}</_array>
				arrayFrag.copy(label=s"$vType${arrayFrag.label}")
			}
			<technique_common>
				<accessor stride={strideLen.toString} count={(contents.length / strideLen).toString} source={s"#$arrayId"}>
					{strideNames.map{name =>
							<param type={vType} name={name}/>
					}}
				</accessor>
			</technique_common>
		</source>
		(id, xmlOut)
	}
	
	private val newGeoms = batchedArrays.zipWithIndex.map{
		case((matUrl, (batchIDs,semVertsDataMap, semVertsConfigMap, indices)), batchI) =>
			val geomId = s"batch$batchI"
			val vertsId = joinId(geomId,"vertex")
			val attributes = (semVertsDataMap.foldLeft(Map[String,Map[(Int,Option[Int]),(String,List[String],List[_ <: AnyVal])]]()){
				case (attrSoFar, (sem@(semN, ofs, semS), data)) =>
					val (vType, vKeys) = semVertsConfigMap(sem)
					attrSoFar + (semN -> (attrSoFar.get(semN).getOrElse(Map()) + ((ofs, semS) -> (vType, vKeys, data))))
			}/* + ("BATCHID" -> Map((semVertsDataMap.size, None) -> ("int", List("I"), batchIDs)))*/).map{
				case (semN,subSem) =>
					(semN -> subSem.map{
						case ((ofs, semS),(vType,vKeys,data)) =>
							assert(data.length > 0)
							(ofs, semS) -> genArrayOfType(data, vKeys, geomId, semN, ofs, semS, vType)
					})
			}
			
			val vertCount = indices.length / 3
			val xmlOut = <geometry id={geomId}>
				<mesh>
					{attributes.flatMap{ _._2.map(_._2._2) }}
					<vertices id={vertsId}>
						<input source={s"#${attributes("VERTEX").head._2._1}"} semantic="POSITION" />
					</vertices>
					<triangles material={matUrl.tail} count={vertCount.toString}>
						{attributes.flatMap{
							case (semN, subSem) => subSem.map{
								case ((ofs, semS),(id,_)) =>
									val (tId, tSem) = (if(semN == "VERTEX") vertsId else id, semN)
									<input offset={ofs.toString} source={s"#$tId"} semantic={tSem}/> % semS.map{
										setV => Attribute("set",Text(setV.toString).theSeq, Null)
									}.getOrElse(Null)
							}
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