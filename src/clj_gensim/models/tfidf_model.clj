(ns clj-gensim.models.tfidf-model
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.models.tfidf-model)

(defn- compute-idfs-from-dfs [dfs totaldocs add]
  (let [idxs (m/non-zero-indices dfs)]
    (m/set-indices! (m/clone dfs)
                    idxs
                    (into-array (m/add! (m/scale! (m/log! (m/select-indices dfs idxs)) -1.0)
                                        (+ add (Math/log totaldocs)))))))

(defrecord TfIdfModel [idfs]
  Transformation
  (init [this s] (init this s {}))
  (init [this s {:keys [add] :or {add 0.0}}]
    (cond (satisfies? DictionaryProtocol s) (assoc this :idfs (compute-idfs-from-dfs (document-frequencies s) (num-documents s) add))
          :else (throw (Exception. (str "Don't know how to create TfIdfModel from " (type s))))))
  (transform [this s]
    (cond (seq? s) (if (empty? s)
                     s
                     (cons (transform this (first s))
                           (lazy-seq (transform this (rest s)))))
          (m/vec? s) (m/mul s idfs)
          :else (throw (Exception. (str "Don't know how to transform " (type s)))))))

(defn tfidf-model [x]
  (init (TfIdfModel. nil) x))


(comment

  (use 'clj-gensim.corpora.dictionary)
  ;; (use 'clj-gensim.corpora.hash-dictionary)

  (def document1 (clojure.string/split "A walk in the park" #" "))
  (def document2 (clojure.string/split "I'm all dressed up tonight" #" "))
  (def document3 (clojure.string/split "A walk tonight ?" #" "))
  (def document4 (clojure.string/split "to walk or not to walk" #" "))
  
  (def dict (dictionary [document1 document2 document3 document4]))
  ;; (def dict (hash-dictionary 2000 [document1 document2 document3 document4]))

  (def tfidf (tfidf-model dict))

  
  (transform tfidf (doc2bow dict document4))
  (transform tfidf (map (partial doc2bow dict)
                        [document1 document2 document3 document4]))
  
  )


