import scala.collection.mutable.ArrayBuffer

/**
 * @author adikou
 */
class Posting(doc: Int) {
  var docID: Int = doc
  val pos: ArrayBuffer[Int] = new ArrayBuffer[Int]
}