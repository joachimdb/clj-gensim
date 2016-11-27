(ns clj-gensim.tutorial.preliminaries  
  (:refer-clojure :exclude [load])
  (:require [clojure.core.matrix :as m]           
            [clojure.java.io :as io]
            [clj-gensim.protocols :refer :all]
            [clj-gensim.models.tfidf-model :refer (tfidf-model)]
            ))
;; (remove-ns 'clj-gensim.tutorial.preliminaries)

;; Tutorial code following the preliminaries at https://radimrehurek.com/gensim/tutorial.html#id1

;; Defining a corpus from token counts:

(def c (corpus [[[0, 1.0], [1, 1.0], [2, 1.0]],
                [[2, 1.0], [3, 1.0], [4, 1.0], [5, 1.0], [6, 1.0], [8, 1.0]],
                [[1, 1.0], [3, 1.0], [4, 1.0], [7, 1.0]],
                [[0, 1.0], [4, 2.0], [7, 1.0]],
                [[3, 1.0], [5, 1.0], [6, 1.0]],
                [[9, 1.0]],
                [[9, 1.0], [10, 1.0]],
                [[9, 1.0], [10, 1.0], [11, 1.0]],
                [[8, 1.0], [10, 1.0], [11, 1.0]]]))

;; Training a tfidf model on a corpus

(def tfidf (tfidf-model c))

;; Transforming a document or a corpus

(def doc (document c [[0 1] [4 1]]))
(transform tfidf doc)

(transform tfidf c)

;; Computing similarities


(cosine-similarity (transform tfidf c) doc)

;; => similarity takes a doc/batch/corpus and a doc and returns the similarities of the doc to ...

;; we'd like to be ablt to write

(def sim (cosine-similarity (norm-model (tfidf-model c))))

;; => models can themselves be corpusses as well?
