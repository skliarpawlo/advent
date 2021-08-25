(ns lucene 
  (:require [clojure.reflect :refer [reflect]])
  (:import [org.apache.lucene.analysis.standard StandardAnalyzer]
           [org.apache.lucene.store RAMDirectory]
           [org.apache.lucene.index IndexWriter IndexWriterConfig]
           [org.apache.lucene.document Document TextField Field$Store]
           [org.apache.lucene.queryparser.classic QueryParser]
           [org.apache.lucene.search TopScoreDocCollector IndexSearcher]
           [org.apache.lucene.index DirectoryReader]))



(def analyzer (StandardAnalyzer.))
(def index (RAMDirectory.))
(def index-write (IndexWriter. index (IndexWriterConfig. analyzer)))

(defn add-doc! [title]
  (let [doc (Document.)]
    (.add doc (TextField. "title" title Field$Store/YES))
    (.addDocument index-write doc)))

(defn index-a-few-docs []
  (add-doc! "New video")
  (add-doc! "Old video about messi")
  (. index-write close))
(index-a-few-docs)

(def query (.parse (QueryParser. "title" analyzer) "messi"))
(def index-searcher (IndexSearcher. (DirectoryReader/open index)))
(def collector (TopScoreDocCollector/create 10 10))

(.search index-searcher query collector)
(def matches (.. collector topDocs scoreDocs))

(-> (first matches)
    .doc
    ((fn [doc-id] (. index-searcher doc doc-id)))
    (.get "title"))


(def x (first matches))
(reflect x)

(2 + 2)