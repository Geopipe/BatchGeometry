package com.geopipe.xml

import scala.xml.Node
import scala.xml.transform.RewriteRule

class RuleApplicator(rule:RewriteRule) extends SaneTransformer {
	override def transform(n: Node): Seq[Node] = rule.transform(super.transform(n))
}