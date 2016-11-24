(ns clj-gensim.models.norm-model
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.models.norm-model)

(defrecord NormModel [norm-type]
  Transformation
  (transform* [this s]
    (condp = norm-type
      "l2" (m/normalise s)
      "l1" (m/scale s (/ 1.0 (m/esum s)))))
  (transform*! [this s]
    (condp = norm-type
      "l2" (m/normalise! s)
      "l1" (m/scale! s (/ 1.0 (m/esum s))))))

(defn norm-model
  ([] (norm-model "l2"))
  ([norm-type]
   (NormModel. norm-type)))


(comment

  (use 'clj-gensim.corpora.dictionary)
  ;; (use 'clj-gensim.corpora.hash-dictionary)
  
  (def dict (dictionary [document1 document2 document3 document4]))
  ;; (def dict (hash-dictionary 2000 [document1 document2 document3 document4]))

  (def norm (norm-model))
  (transform norm (document dict document4))
  (transform norm (document dict document1))

  (use 'clj-gensim.models.tfidf-model)

  (def tfidf (tfidf-model dict))
  (transform norm (transform tfidf (document dict document4)))
  (transform norm (transform tfidf (document dict document1)))
  
  (transform norm
             (transform tfidf
                        (map (partial document dict)
                             [document1 document2 document3 document4])))
  
  )


