(ns clj-gensim.models.norm-model
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.models.norm-model)

(defrecord NormModel [norm-type]
  ModelProtocol
  (transform-document [this doc]
    (condp = norm-type
      "l2" (m/normalise doc)
      "l1" (m/scale doc (/ 1.0 (m/esum doc)))))
  (transform-batch [this b]
    ;; TODO: there must be a more efficient (low-level) way of doing this?
    (reduce m/conjoin (empty this) (transform-document this (documents b))))
  (train-document [this doc] this)
  (train-batch [this b] this))

(defn norm-model
  ([] (norm-model "l2"))
  ([norm-type]
   (NormModel. norm-type)))


(comment

  (use 'clj-gensim.corpora.dictionary)
  ;; (use 'clj-gensim.corpora.hash-dictionary)

  (def texts [{:text "A walk in the park" :language :english}
              {:text "I'm all dressed up tonight" :language "en"}
              {:text "A walk tonight ?" :language "en"}
              {:text "to walk or not to walk" :language "en"}])
  
  (def dict (dictionary texts))

  (def norm (norm-model))

  (transform norm (document dict (last texts)))
  (transform norm (document dict (first texts)))
  (transform norm (map (partial document dict) texts))
  
  )
