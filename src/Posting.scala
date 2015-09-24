import scala.collection.mutable.ArrayBuffer

/**
 * @author adikou
 */
class Posting(doc: Int, posit: ArrayBuffer[Int]) {
    var docID: Int = doc
    val pos: ArrayBuffer[Int] = posit
	def this(doc: Int) = this(doc, new ArrayBuffer[Int]())    
}