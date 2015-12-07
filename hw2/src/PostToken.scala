/**
 * @author adikou
 */
class PostToken(tk: String, doc: Int, pos: Int) {
  var token: String = tk
  var docID: Int = doc
  var position: Int = pos
  def this() = this("",0,0)
  def equate(that: PostToken) = {
    this.token = that.token
    this.docID = that.docID
    this.position = that.position
  }
}