(ns clj-gensim.corpora.matrix-corpus
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.matrix-corpus)

(defrecord MatrixCorpus [m]
  Corpus
  (num-documents [this] (first (m/shape m)))
  (num-nonzero [this] (m/non-zero-count m))
  (document-at [this idx] (m/sparse (m/slice m idx)))
  (document-matrix [this] m)
  (documents [this]
    (map m/sparse (m/slices m)))
  (add-document [this doc]
    (update-in this [:m] #(m/sparse (m/conjoin %1 %2)) doc)))

(defn matrix-corpus [docs]
  (add-documents (MatrixCorpus. (m/new-array [0 (first (m/shape (first docs)))])) docs))

(comment

  
  (use 'clj-gensim.corpora.dictionary)

  (def dict (dictionary [document1 document2 document3 document4]))
  (def docs (map (partial document dict)
                 [document1 document2 document3 document4]))
  (def c (matrix-corpus docs))
  (num-documents c)
  (documents c)
  (document-at c 1)
  (document-matrix c)

  (m/sparse? (document-matrix c))
  (m/sparse? (first (documents c)))
  (m/sparse? (document-at c 1))

  )

