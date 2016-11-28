(ns clj-gensim.similarities.cosine-similarity
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.similarities.cosine-similarity)

(defrecord CosineSimilarity [c]
  SimilarityProtocol
  (sim-document [this doc] (map #(vector %1 (m/inner-product %2 doc)) (range) (documents c))))

(defn construct-cosine-similarity [corpus]
  (CosineSimilarity. corpus))

(defn cosine-similarity [c]
  (let [s (construct-cosine-similarity c)]
    (fn [doc] (sim s doc))))


