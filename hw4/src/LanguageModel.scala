import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import scala.collection.mutable.ArrayBuffer
import jline.console.ConsoleReader

object LanguageModel {
    def main(args:Array[String]): Unit = {
        
        val condProb = (t: String, set: Array[String]) => set.foldLeft(0)(
                            (count, term) => if(term.equals(t)) count + 1
                                             else count
                        ) / set.length.toDouble

        val docLines = io.Source.fromFile("resources/document.txt").getLines.toList
        val docs = new ArrayBuffer[ArrayBuffer[String]]()
        val collection = new ArrayBuffer[String]()
        val processor = new FastNLPProcessor()
        
        for(doc <- docLines) {
            val document = processor.annotate(doc.substring(doc.indexOf(":")+1, doc.length))
            //val document = processor.annotate(doc)
            docs += new ArrayBuffer[String]()
            for(sentence <- document.sentences) {
                docs(docs.length-1) ++= sentence.lemmas.get.foldLeft(Array[String]()) {
                                            (a, b) => if(b.matches("(\\w+)")) a :+ b else a
                                        }
                collection ++= docs(docs.length-1)
            }
            //println(docs.mkString(" "))
            //println(collection.mkString(" "))
        }

        val docScores = new Array[Double](docs.length)
        val lambda = 0.5
        val reader = new ConsoleReader

        val doSmooth = true

        reader.setPrompt("\nType 'q' or 'quit' to exit. \nEnter query >> ")
        var query = reader.readLine()

        while(!query.equals("q") && !query.equals("quit")) {
            val termDocs = processor.annotate(query)
            for(sentence <- termDocs.sentences) {
                for(i <- 0 to docs.length-1) {
                    var score = 1.0
                    for(term <- sentence.lemmas.get)  {
                        if(doSmooth)
                            score *= (lambda * condProb(term, docs(i).toArray)
                                 + (1 - lambda) * condProb(term, collection.toArray))
                        else score *= (condProb(term, docs(i).toArray)
                                     + condProb(term, collection.toArray)) 
                    }
                    docScores(i) = score
                }

            }

            var scoreMap = new scala.collection.immutable.TreeMap[Double, Int]()
            for(i <- 0 to docScores.length-1) scoreMap += (docScores(i) -> (i+1))
            println(scoreMap.toList.reverse.mkString("\n"))

            query = reader.readLine()
        }
    }
}
