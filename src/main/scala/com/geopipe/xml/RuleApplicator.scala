package com.geopipe.xml

import scala.xml.{Node, Elem}

object RuleApplicator {
	def apply(rules:PartialFunction[Node, Node]*) = {
		val rule = rules.reduceLeft(_ orElse _)
		new RuleApplicator(rule)
	}
}

class RuleApplicator private (rule:PartialFunction[Node, Node]) {
	def apply(n: Node): Node = {
		rule.applyOrElse[Node,Node](n, identity[Node]) match {
			case e:Elem => e.copy(child = e.child.map(this(_)))
			case other => other
		}
	}
}