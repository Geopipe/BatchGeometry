package com.geopipe.profiling

object TicToc {
	private var stack = List[Long]()
	val ELIDE_BELOW_DEPTH = 2
	def tic:Unit = synchronized {
		stack = System.currentTimeMillis +: stack
		if(stack.length <= ELIDE_BELOW_DEPTH) Console.println(s"${"\t" * stack.length}tic")
	}
	
	def toc(tag: =>String):Unit = synchronized {
		val now = System.currentTimeMillis
		if(stack.length <= ELIDE_BELOW_DEPTH) Console.println(s"${"\t" * stack.length}toc($tag): ${now - stack.head}ms")
		stack = stack.tail
	}
}