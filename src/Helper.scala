object Helper {
	def splitOp(x: String) = x.replace("c", " c ").replace("u", " u ").split(" ")
  	def processOp(x: String) = x.replaceAll("[0-9]","").toLowerCase
}