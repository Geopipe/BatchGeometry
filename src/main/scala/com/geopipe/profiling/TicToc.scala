package com.geopipe.profiling

object TicToc {
	private var stack = List[Long]()
	def tic:Unit = synchronized {
		stack = System.currentTimeMillis +: stack
		Console.println(s"${"\t" * stack.length}tic")
	}
	
	def toc(tag:String):Unit = synchronized {
		val now = System.currentTimeMillis
		Console.println(s"${"\t" * stack.length}toc($tag): ${now - stack.head}ms")
		stack = stack.tail
	}
}