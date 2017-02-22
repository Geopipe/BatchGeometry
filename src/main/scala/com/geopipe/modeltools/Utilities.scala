package com.geopipe.modeltools


import scala.xml._
import scala.language.implicitConversions



object MiscHelpers {
	implicit class OptionableBoolean(b:Boolean) {
		def toOption[A](condV: =>A):Option[A] = {
			if(b) Some(condV) else None
		}
		
		def withOption[A](condV: => Option[A]):Option[A] = {
			if(b) condV else None
		}
	}
	
	def retrieveSceneNodes(collada:Node):Seq[Node] = {
		collada \ "library_visual_scenes" \ "visual_scene" \ "node"
	}
	
	def toIdDict(ns:NodeSeq) = ns.collect{case e:Elem => (e \@ "id" -> e)}.toMap
	
	object FragURL {
		def unapply(s:String):Option[String] = (s(0) == '#').toOption(s.tail)
	}
}

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

		def copyEnhanced(maybePre:Option[String] = this.maybePre, key:String = this.key, value:Either[String,Seq[Node]] = Right(this.value)):MetaDataEnhanced = metadata match {
			case Null => throw new IllegalStateException("Can't copy Null metadata with new pre:key=value")
			case _ => Attribute(maybePre, key, value.left.map(Text(_).theSeq).merge, metadata.next)
		}
	}

	implicit def enhanceMetaData(md:MetaData): MetaDataEnhanced = new MetaDataEnhanced(md)
	implicit def iterableToMetaData(items: Iterable[MetaData]): MetaData = items match {
		case Nil => Null
		case head :: tail => head.copy(next=iterableToMetaData(tail))
	}
}

object ReplacementHelpers {
	import MiscHelpers._
	def replaceAll(initText:String, replacements:Map[String,String]):Option[String] = {
			val newText = replacements.toSeq.sortBy( - _._1.length).foldLeft(initText){
				(srcText,kvPair) => srcText.replace(kvPair._1, kvPair._2)
			}
			(newText != initText).toOption(newText)
	}

	def mapWhereDefined(children:Seq[Node],f:Node => Option[Node]):Seq[Node] = {
		children.map{ child => f(child).getOrElse(child) }
	}
}