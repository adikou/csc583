
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * @author adikou
 */
object BoolQuery {
    
    def map(words: ArrayBuffer[Posting]) = {
        val terms = HashMap.empty[String, ListBuffer[Int]]
        for(word <- words) {
          if(terms.contains(word.token)) {
              if(!terms(word.token).contains(word.docID))
                  terms(word.token) += word.docID
          }
          else {
            terms += (word.token -> new ListBuffer[Int])
            terms(word.token) += word.docID
          }
        }
        terms
    }
    
    def tokenize(lines: List[String]): ArrayBuffer[Posting] = {
        val words = new ArrayBuffer[Posting]()
        var id = 1

        for(line <- lines) {
            val tokens = line.split(" ")
            for(i <- 1 until tokens.length)
                words += new Posting(tokens(i).toLowerCase, id)
            id += 1
        }
        words
    }
    
    def constructInvertedIndex(file: String) = {
        val lines = Source.fromFile(file).getLines.toList
        val words = BoolQuery.tokenize(lines)
        CustomMergeSort.mergesort(words, 0, words.length-1)
        val index = BoolQuery.map(words)
        index
    }
    
    
    def AND(p1: List[Int], p2: List[Int]): List[Int] = {
        val result = new ListBuffer[Int]
        val it1 = p1.iterator.buffered
        val it2 = p2.iterator.buffered
        
        while(!it1.isEmpty && !it2.isEmpty) {
            if(it1.head == it2.head) {
                result += it1.head
                it1.next
                it2.next
            }
            else if(it1.head < it2.head) it1.next
            else it2.next
        }
        result.toList
    }
    
    def OR(p1: List[Int], p2: List[Int]): List[Int] = {
        val result = new ListBuffer[Int]
        val it1 = p1.iterator.buffered
        val it2 = p2.iterator.buffered
        
        while(!it1.isEmpty && !it2.isEmpty) {
            if(it1.head == it2.head) {
                result += it1.head
                it1.next
                it2.next
            }
            else if(it1.head < it2.head) result += it1.next
            else result += it2.next
        }
        
        val rest = if(it1.isEmpty) it2 else it1
        while(rest.hasNext)
            result += rest.next
        result.toList
        
    }
    
    def main(args: Array[String]) = {
        
        val index = BoolQuery.constructInvertedIndex(args(0))
        /*index.foreach((x) => println(x._1 + " -> " + x._2.mkString(" ")))
         *val res = BoolQuery.OR(index("new").toList, index("patients").toList)
         *println(res.mkString(" -> "))
         */
         
    }  
}