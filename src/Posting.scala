

/**
 * @author adikou
 */
class Posting(tk: String, doc: Int) {
  var token: String = tk
  var docID: Int = doc
  def this() = this("",0)
  def equate(that: Posting) = {
    this.token = that.token
    this.docID = that.docID
  }
}