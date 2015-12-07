import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.search.similarities.DefaultSimilarity;
import org.apache.lucene.search.similarities.BM25Similarity;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.RAMDirectory;
import org.apache.lucene.util.Version;

import java.io.IOException;


object LuceneQuery {
    object SIM extends Enumeration {
        type SIM = Value
        val TFIDF, OKAPIBM25 = Value
    }

    def scanDocuments(fileLines: List[String], w: IndexWriter) = {
        fileLines foreach { 
            line => addDoc(w, line, line.substring(0, line.indexOf(":")))
        }
    }

    def main(args: Array[String]) = {
        val index = new RAMDirectory()
        val analyzer = new StandardAnalyzer()
        var config = new IndexWriterConfig(analyzer)
        var w = new IndexWriter(index, config)
        val fileLines = io.Source.fromFile("resources/document.txt").getLines.toList

        import SIM._
        var currentSimilarity = TFIDF

        scanDocuments(fileLines, w)
        w.close()

        val hitsPerPage = 10
        val reader = DirectoryReader.open(index)

        while(true) {
            println("\nLucene :: Enter 'q' or 'quit' to exit program")
            print("          Enter 'changesim' to change similarity to ")
            if(currentSimilarity == OKAPIBM25) println("TFIDF (Default)") else println("Okapi BM25")
            val queryLine = scala.io.StdIn.readLine("\nEnter query > ")
            if(queryLine == null) println("Enter non-empty query string")
            else if(queryLine.equals("q") || queryLine.equals("quit"))
                System.exit(0)
            else if(queryLine.equals("changesim")) {
                currentSimilarity = if(currentSimilarity == TFIDF) OKAPIBM25 else TFIDF
                print("Changed similarity to ") 
                if(currentSimilarity == TFIDF) println("TFIDF (Default)") else println("Okapi BM25")
                config = new IndexWriterConfig(analyzer)
                if(currentSimilarity == OKAPIBM25) config.setSimilarity(new BM25Similarity())
                    else config.setSimilarity(new DefaultSimilarity())

                w = new IndexWriter(index, config)
                scanDocuments(fileLines, w)
                w.close()
            }
            else {
                val query = new QueryParser("fileContent", analyzer).parse(queryLine)
                val searcher = new IndexSearcher(reader)
                val collector = TopScoreDocCollector.create(hitsPerPage)

                if(currentSimilarity == OKAPIBM25)
                    searcher.setSimilarity(new BM25Similarity())
                else searcher.setSimilarity(new DefaultSimilarity())

                searcher.search(query, collector)
                val hits = collector.topDocs().scoreDocs

                print("Found " + hits.length)
                if(hits.length == 1) println(" hit") else println(" hits")
                hits foreach {
                    hit => {
                        val d = searcher.doc(hit.doc)
                        println(d.get("docName") + "\t" + hit.score)
                    }
                }
            }
        }

        reader.close()

    }

    @throws(classOf[IOException])
    def addDoc(w: IndexWriter, fileContent: String, docName: String) = {
        val doc = new Document()
        doc.add(new TextField("fileContent", fileContent, Field.Store.YES))
        doc.add(new StringField("docName", docName, Field.Store.YES))
        w.addDocument(doc)
    }
}