(ns clj-gensim.tutorial.preliminaries  
  (:refer-clojure :exclude [load])
  (:require [clojure.core.matrix :as m]           
            [clojure.java.io :as io]
            [clj-gensim.protocols :refer :all]
            [clj-gensim.corpora.matrix-corpus :refer (matrix-corpus)]
            [clj-gensim.models.tfidf-model :refer (tfidf-model)]))
;; (remove-ns 'clj-gensim.tutorial.preliminaries)

;; Tutorial code following the preliminaries at https://radimrehurek.com/gensim/tutorial.html#id1

;; defining a simple corpus as below

(def corpus (matrix-corpus [[[0, 1.0], [1, 1.0], [2, 1.0]],
                            [[2, 1.0], [3, 1.0], [4, 1.0], [5, 1.0], [6, 1.0], [8, 1.0]],
                            [[1, 1.0], [3, 1.0], [4, 1.0], [7, 1.0]],
                            [[0, 1.0], [4, 2.0], [7, 1.0]],
                            [[3, 1.0], [5, 1.0], [6, 1.0]],
                            [[9, 1.0]],
                            [[9, 1.0], [10, 1.0]],
                            [[9, 1.0], [10, 1.0], [11, 1.0]],
                            [[8, 1.0], [10, 1.0], [11, 1.0]]]))

(def tfidf (tfidf-model corpus))

;;; now for the part
 ;; >>> index = similarities.SparseMatrixSimilarity(tfidf[corpus], num_features=12)
;; >>> sims = index[tfidf[vec]]
;; >>> print(list(enumerate(sims)))
;; [(0, 0.4662244), (1, 0.19139354), (2, 0.24600551), (3, 0.82094586), (4, 0.0), (5, 0.0), (6, 0.0), (7, 0.0), (8, 0.0)]

;; the following returns a sequence of tfidf vectors
(transform tfidf corpus)
;; What we'd like however is another matrix corpus holding the tfidf vectors as a matrix

;; next we need to define a similarity protocol or sth that computes cosine distances between two documents or a corpus and a document or two corpora etc...

