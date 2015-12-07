import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.io.Source
import java.nio.file.{Files, FileSystems}

/**
 * @author adikou
 * 09/18/15: Design change. Migrating all lists
 * to arrays. Appending is amortized constant 
 * and access is O(1). Only penalty *might* be
 * prepending to this list. Don't see that 
 * happening any time soon. 
 * 
 * @TODO Add modularization of methods by splitting
 *       into query classes, functional, and main classes.
 */

object BoolQuery {
  
    /* Currently the inverted index is built upon 
     * every run of the program. Further work is to
     * provide option to start a new index, or add
     * documents to append to current index.
     */
  
    private var index = HashMap.empty[String, ArrayBuffer[Posting]]
    private val cmds = Array("build", "query", "help", "quit")
    private val ops = Map("\\and" -> 1, "\\or" -> 2, "\\u" -> 3,
                          "\\d" -> 4)
    private val opPrecedence = Predef.Map("\\and" -> 2, "\\u"  -> 2, 
                                          "\\d"   -> 2, "\\or" -> 1)

    private val docIDToName = new ArrayBuffer[String]

    private val err = """|Unrecognized command. Type 'help' for a list
                         |of commands""".stripMargin.replaceAll("\n", " ")
    private val cmd_help =  "\nbuild <path/to/DOCUMENT>\n"
                           .concat("query <QUERY> [note: Use \\and,") 
                           .concat(" \\or for logical ")
                           .concat("operators]\n")
                           .concat("quit\n")
      
    
    def map(words: ArrayBuffer[PostToken]) = {
        val terms = HashMap.empty[String, ArrayBuffer[Posting]]
        for(word <- words) {
            if(terms.contains(word.token)) {
                val t = terms(word.token)
                if(t.exists(T => T.docID == word.docID))
                    t(t.length - 1).pos += word.position
                else {
                    t += new Posting(word.docID)
                    t(t.length - 1).pos += word.position      
                }
          }
          else {
              terms += (word.token -> new ArrayBuffer[Posting])
              val t = terms(word.token)
              t += new Posting(word.docID)
              t(t.length-1).pos += word.position
          }
        }

        terms
    }
    
    def tokenize(lines: List[String]): ArrayBuffer[PostToken] = {
        val words = new ArrayBuffer[PostToken]()
        var id = 0

        for(line <- lines) {
            var pos = 1
            val tokens = line.split(" ").toArray
            this.docIDToName += tokens(0)
            for(i <- 1 until tokens.length){
                words += new PostToken(tokens(i).toLowerCase, id, pos)
                pos += 1
            }
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
    
    /*
     * Essentially legacy code, since it doesn't deal with 
     * positions. Translating both OR, and AND for dealing
     * with legacy \and and \or queries.
     *
     * @TODO: Design question. Add reference of iterator to
     * result or add a new copy? O(n) space for latter, but
     * messy positions list added to the result. Ideally, we
     * need only a list of docIDs. Copying references can
     * potentially copy the positions list as well, which we 
     * really don't need.
     * 
     * 09/24/15 - Switched from buffered iterator to index based 
     *            array access. Originally, horribly space-inefficient
     *            O(n) to the length of the arrays, when buffered.
     */

    def AND(p1: Array[Posting], p2: Array[Posting]): Array[Posting] = {
        val result = new ArrayBuffer[Posting]
        var it1 = 0
        var it2 = 0

        val N1 = p1.length
        val N2 = p2.length
        
        while(it1 < N1 && it2 < N2) {
            if(p1(it1).docID == p2(it2).docID) {
                result += p1(it1)
                it1 += 1
                it2 += 1
            }
            else if(p1(it1).docID < p2(it2).docID) it1 += 1
            else it2 += 1
        }
        result.toArray
    }
    
    def OR(p1: Array[Posting], p2: Array[Posting]): Array[Posting] = {
        val result = new ArrayBuffer[Posting]
        var it1 = 0
        var it2 = 0

        val N1 = p1.length
        val N2 = p2.length
        
        while(it1 < N1 && it2 < N2) {
            if(p1(it1).docID == p2(it2).docID) {
                result += p1(it1)
                it1 += 1
                it2 += 1
            }
            else if(p1(it1).docID < p2(it2).docID) {
                    result += p1(it1)
                    it1 += 1
            }
            else {
                result += p2(it2)
                it2 += 1
            }
        }
        
        while(it1 < N1) {
            result += p1(it1)
            it1 += 1
        }
        while(it2 < N2) {
            result += p2(it2)
            it2 += 1
        }

        result.toArray
        
    }
    
    /* 
     * Positional Intersect. Implemented as per Manning et al.
     * 09/18/15 - Agnostic to direction of 'k'
     * 09/24/15 - After week of absence, implemented directional query.
     */
    def POSITIONAL_AND(p1: Array[Posting], p2: Array[Posting], 
                       k: Int, isDirectional: Boolean): Array[Posting] = {
        val result = new ArrayBuffer[Posting]
        var it1 = 0
        var it2 = 0

        val N1 = p1.length
        val N2 = p2.length

        while(it1 < N1 && it2 < N2) {
            if(p1(it1).docID == p2(it2).docID) {
                
                /*
                 * Note: Choosing between List and ListBuffer is no choice.
                 * Appending to List is O(n) while l.tail is O(1)
                 * Appending to ListBuffer is O(1) but tail is O(n).
                 */

                var l = List[Int]()
                val pp1 = p1(it1).pos
                val pp2 = p2(it2).pos

                var _it1 = 0
                var _it2 = 0

                val _N1 = pp1.length
                val _N2 = pp2.length

                object e extends Exception {}
                
                while(_it1 < _N1) {
                    try{
                        while(_it2 < _N2) {
                        	if(!isDirectional) {
	                            if(math.abs(pp1(_it1) - pp2(_it2)) <= k)
	                                l ++= List(pp2(_it2))
	                            else if(pp2(_it2) > pp1(_it1))
	                                throw e
	                            _it2 += 1
                        	}
                        	else {
                        		if(pp1(_it1) + k >= pp2(_it2)) {
                                    if(pp2(_it2) > pp1(_it1))
                                        l ++= List(pp2(_it2))
                                    _it2 += 1
                        		}
                        		else throw e
                        	}
                        }
                    } catch {
                    case e: Exception => 
                    } 

                    if(!isDirectional) {
                        while(l.length != 0 && math.abs(l.head - pp1(_it1)) > k)
                            l = l.tail
                    }

                    else {
                        while(l.length != 0 && (pp1(_it1) + k < l.head))
                            l = l.tail
                    } 

                    l.foreach((a: Int) => result += new Posting(p1(it1).docID,
                                                    ArrayBuffer(pp1(_it1), a)))
                    _it1 += 1
                }

                it1 += 1
                it2 += 1
            }
            else if(p1(it1).docID < p2(it2).docID) it1 += 1
            else it2 += 1
        }

        result.toArray
    } 

    /* Simple Dijkstra's Shunting Yard Algorithm to 
     * convert the infix query to RPN notation
     * */
    
    def parseQuery(query: Array[String]): Array[String] = {
        val rpnQuery = new Queue[String]
        val opStack  = new Stack[String]
        for(token <- query) {
            if(ops.contains(Helper.processOp(token))) {
                val t = Helper.processOp(token)
                while(opStack.length > 0 
                     && opPrecedence.contains(Helper.processOp(opStack.top))
                     && (opPrecedence(t) <= opPrecedence(
                         Helper.processOp(opStack.top)))) {
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
            else rpnQuery.enqueue(token.toLowerCase)
        }
        
        while(opStack.length > 0)
            rpnQuery.enqueue(opStack.pop)
            
        rpnQuery.toArray
    }
    
    /*
     * Stack based evaluation of RPN notation
     * */
    
    def evaluateQuery(query: Array[String]): Unit = {
        val op = new Stack[Array[Posting]]
        val N = query.length
        if(N > 0) {
            val flag = if(ops.contains(Helper.processOp(query(N-1))))
                            ops(Helper.processOp(query(N-1))) > 2
                        else false
            for(token <- query) {
                if(ops.contains(Helper.processOp(token))) {
                    if(op.length > 0) {
                        ops(Helper.processOp(token)) match {
                            case 1 => op.push(AND(op.pop, op.pop))
                            case 2 => op.push(OR (op.pop, op.pop))
                        
                            /* Positional for now. When more ops are
                             * included, add more dissection options
                             */ 
                            case _ => val x = Helper.splitOp(token)
                            		  val op1 = op.pop
                            		  val op2 = op.pop
                                      op.push(POSITIONAL_AND(op2, op1,
                                              x(2).toInt, x(1) == "d"))
                        }
                    }
                }
                else if(index.contains(token))
                    op.push(index(token).toArray)
                else {
                    println("Error: " + token + ": no such term/operator")
                    return
                }
            }
            
            if(op.top.length == 0) println("NIL")
            else {
                val it = op.top.iterator.buffered
                while(it.hasNext) {
                    print(docIDToName(it.head.docID))
                    val _it = it.next.pos.iterator
                    if(flag) print(" : ") 
                    else if(it.hasNext) print(" -> ")
                         else println()
                    if(flag) {
                    	print("(")
                        while(_it.hasNext) {
                            print(_it.next)
                            if(_it.hasNext) print(", ") else println(")")
                        }
                    }
                }
            }
        }
    }
    
    def main(args: Array[String]) = {
        
        println("BoolQuery v0.1")
        println("Type 'help' for a list of commands")
        
        //BoolQuery.constructInvertedIndex(args(0))
        while(true) {
            val cmdLine = scala.io.StdIn.readLine("\nBoolQuery> ")
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