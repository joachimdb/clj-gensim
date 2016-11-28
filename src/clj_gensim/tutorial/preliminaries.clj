(ns clj-gensim.tutorial.preliminaries  
  (:refer-clojure :exclude [load])
  (:require [clj-gensim.protocols :refer :all]
            [clj-gensim.models.tfidf-model :refer (tfidf-model)]
            [clj-gensim.models.norm-model :refer (norm-model)]
            [clj-gensim.similarities.cosine-similarity :refer (cosine-similarity)]))
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

;; Training a tfidf model on a corpus:
(def tfidf (tfidf-model c))

;; Transforming a document or a corpus:
(def doc (document c [[0 1] [4 1]]))
(tfidf doc)
(tfidf c)

;; Chaining transformations:
(def normalize (norm-model))
(normalize doc)
(normalize (tfidf doc))
(normalize (tfidf c))

;; Computing similarities:
(def csim (cosine-similarity (normalize (tfidf c))))
(csim (normalize doc))
(csim (normalize (first (documents c))))

