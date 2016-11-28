(ns clj-gensim.tutorial.corpora-and-vector-spaces
  (:refer-clojure :exclude [load])
  (:require [clojure.core.matrix :as m]           
            [clojure.java.io :as io]
            [clj-gensim.protocols :refer :all]
            [clj-gensim.corpora.dictionary :refer (dictionary)]
            [clj-gensim.models.tfidf-model :refer (tfidf-model)]
            ))

;;; This time, we start from documents represented as strings:
(def docs [{:language "en" :text "Human machine interface for lab abc computer applications"},
           {:language "en" :text "A survey of user opinion of computer system response time"},
           {:language "en" :text "The EPS user interface management system"},
           {:language "en" :text "System and human system engineering testing of EPS"},
           {:language "en" :text "Relation of user perceived response time to error measurement"},
           {:language "en" :text "The generation of random binary unordered trees"},
           {:language "en" :text "The intersection graph of paths in trees"},
           {:language "en" :text "Graph minors IV Widths of trees and well quasi ordering"},
           {:language "en" :text "Graph minors A survey"}])

;;; clj-gensim provides built-in language dependent lucene-based tokenization:
(tokens (first docs))

;;; Constructing a dictionary for the tokens in the documents:
(def dict (dictionary docs))
(:token2id dict)

;;; The dictionary can be used to compute vector representations of texts:
(document dict {:text "Human computer interaction" :language "en"})

;;; Create a corpus from the documents using the dictionary:
(def c (corpus dict docs {}))
(documents c)

;;; Any sequence of documents is a corpus:
(def mycorpus (map #(document dict {:text % :language "en"})
                   (line-seq (io/reader (io/resource "tutorial/mycorpus.txt")))))
(documents mycorpus)

;;; The dictionary can also be built in a streaming fashion:
(def dict (dictionary (map #(hash-map :text % :language "en")
                           (line-seq (io/reader (io/resource "tutorial/mycorpus.txt"))))))
