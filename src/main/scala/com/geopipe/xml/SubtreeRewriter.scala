package com.geopipe.xml

import scala.xml.{Node, Elem}

object SubtreeRewriter {
	def apply(rules:PartialFunction[Node, Node]*) = {
		val rule = rules.reduceLeft(_ orElse _)
		new SubtreeRewriter(rule)
	}
}

/* We don't descend after we replace a subtree */
class SubtreeRewriter private (rule:PartialFunction[Node, Node]) {
	def apply(n: Node): Node = {
		rule.applyOrElse[Node,Node](n, {
			case e:Elem => e.copy(child = e.child.map(this(_)))
			case other => other
		})
	}
}