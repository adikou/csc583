
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.io.Source
import java.nio.file.{Files, FileSystems}

/**
 * @author adikou
 */
object BoolQuery {
  
    /* Currently the inverted index is built upon 
     * every run of the program. Further work is to
     * provide option to start a new index, or add
     * documents to append to current index.
     */
  
    private var index = HashMap.empty[String, ListBuffer[Int]]
    private val cmds = Array("build", "query", "help", "quit")
    private val ops = Array("\\AND", "\\OR")
    private val opPrecedence = Predef.Map("\\AND" -> 2, "\\OR" -> 1)
    private val opFuncMap = Predef.Map("\\AND" -> {this.AND(_,_)},
                                       "\\OR"  -> {this.OR (_,_)})
    private val err = """|Unrecognized command. Type 'help' for a list
                         |of commands""".stripMargin.replaceAll("\n", " ")
    private val cmd_help =  "\nbuild <path/to/DOCUMENT>\n"
                           .concat("query <QUERY> [note: Use \\and,") 
                           .concat(" \\or for logical ")
                           .concat("operators]\n")
                           .concat("quit\n")
      
    
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
        var flag = true
        if(Files.isDirectory(FileSystems.getDefault().getPath(file))) {
            println(file + " is a directory. Enter valid filepath")
            flag = false
        }
        else if(!Files.exists(FileSystems.getDefault().getPath(file))) {
            println(file + " does not exist. Enter valid filename")
            flag = false
        }
        
        if(flag) {
            val lines = Source.fromFile(file).getLines.toList
            val words = BoolQuery.tokenize(lines)
            CustomMergeSort.mergesort(words, 0, words.length-1)
            this.index = BoolQuery.map(words)
        }
        
        flag
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
    
    
    /* Simple Dijkstra's Shunting Yard Algorithm to 
     * convert the infix query to RPN notation
     * */
    
    def parseQuery(query: Array[String]): Array[String] = {
        val rpnQuery = new Queue[String]
        val opStack  = new Stack[String]
        for(token <- query) {
            if(ops.contains(token.toUpperCase)) {
                while(opStack.length > 0 && opPrecedence.contains(opStack.top)
                     && (opPrecedence(token) <= opPrecedence(opStack.top))) {
                    rpnQuery.enqueue(opStack.pop)
                }
                opStack.push(token)
            }
            else if(token.equals("("))
                opStack.push(token)
            else if(token.equals(")")) {
                while(opStack.length > 0 && !opStack.top.equals("("))
                    rpnQuery.enqueue(opStack.pop)
                if(opStack.length == 0){
                    println("Syntax error - missing (")
                    rpnQuery.clear()
                    return rpnQuery.toArray
                }
                opStack.pop                    
            }
            else rpnQuery.enqueue(token)
        }
        
        while(opStack.length > 0)
            rpnQuery.enqueue(opStack.pop)
            
        rpnQuery.toArray
    }
    
    /*
     * Stack based evaluation of RPN notation
     * */
    
    def evaluateQuery(query: Array[String]): Unit = {
        val op = new Stack[List[Int]]
        if(query.length > 0) {
          for(token <- query) {
              if(ops.contains(token.toUpperCase)) {
                  if(op.length > 0)
                      op.push(opFuncMap(token.toUpperCase)(
                              op.pop, op.pop))
              }
              else if(index.contains(token))
                  op.push(index(token).toList)
              else {
                  println("Error: " + token + ": no such term/operator")
                  return
              }
          }
          if(op.top.length == 0) println("NIL")
              else println(op.top.mkString(" -> "))
        }
    }
    
    def main(args: Array[String]) = {
        
        println("BoolQuery v0.1")
        println("Type 'help' for a list of commands")
        
        //BoolQuery.constructInvertedIndex(args(0))
        while(true) {
            val cmdLine = scala.io.StdIn.readLine("BoolQuery> ")
            if(cmdLine == null) 
                System.exit(0)
            val cmdArgs = cmdLine.replaceAll("[(]", "( ")
                                 .replaceAll("[)]", " )").split(" ").toArray
            cmds.indexOf(cmdArgs(0)) match {
              case 0 => if(BoolQuery.constructInvertedIndex(cmdArgs(1)))
                            println("Inverted index constructed for " 
                                    + cmdArgs(1))
              case 1 => 
                        BoolQuery.evaluateQuery(
                          (BoolQuery.parseQuery(
                                       cmdArgs.slice(1, cmdArgs.length+1)
                                     )
                          )
                        )
              case 2 => println (cmd_help)
              case 3 => System.exit(0)
              case _ => println (err)
            } 
            
        }
    }  
}