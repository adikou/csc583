object Helper {
	def splitOp(x: String) = x.replace("d", " d ").replace("u", " u ").split(" ")
  	def processOp(x: String) = x.replaceAll("[0-9]","").toLowerCase
}
