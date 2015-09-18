import scala.collection.mutable.ArrayBuffer

/**
 * @author adikou
 */
object CustomMergeSort {
     
    // Mergesort is the only decent stable sort.
    // Forget scala.util.Sorting to help out.
    def merge(A: ArrayBuffer[PostToken],
              p: Int, q: Int, r: Int): ArrayBuffer[PostToken] = {
        val n1 = q-p+1
        val n2 = r-q
        val L = new ArrayBuffer[PostToken]
        val R = new ArrayBuffer[PostToken]
        for(i <- 0 until n1) { 
            L += new PostToken
            L(i).equate(A(p+i))
        }
        for(j <- 0 until n2) {
            R += new PostToken
            R(j).equate(A(q+j+1))
        }
        var i = 0
        var j = 0
        var k = p
        
        while(i < n1 && j < n2) {
          if(L(i).token <= R(j).token) {
              A(k).equate(L(i))
              i += 1
          }
          else {
              A(k).equate(R(j))
              j += 1
          }
          k += 1
        }
        // Fill the rest of the array when one index goes out of bounds
        if(i >= n1) {
          while(j < n2) {
            A(k).equate(R(j))
            j += 1
            k += 1
          }
        }
        else {
          while(i < n1) {
            A(k).equate(L(i))
            i += 1
            k += 1
          }
        }
        A
    }
    
    def mergesort(A: ArrayBuffer[PostToken],
                  p: Int, r: Int): ArrayBuffer[PostToken] = {
        if(p < r) {
            val q = (p+r)/2
            mergesort(A,p,q)
            mergesort(A,q+1,r)
            merge(A,p,q,r)
        }
        A
    }
}