(ns clj-gensim.corpora.hash-dictionary
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.hash-dictionary)

(defn- token-hash [max-hash f]
  (let [r (rem (.hashCode f) max-hash)]
    (if (< r 0)
      (+ r max-hash)
      r)))

(defrecord HashDictionary [max-hash dfs num-docs num-pos num-nnz]
  DictionaryProtocol
  (index [this token] (token-hash max-hash token))
  (document-frequencies [this] dfs)
  (num-documents [this] num-docs)
  (num-tokens [this] num-pos)
  (num-unique-tokens [this] num-nnz)  
  (doc2bow [this tokens]
    (let [ret (m/new-sparse-array max-hash)]
      (doseq [[idx s] (group-by identity (remove nil? (map #(index this %) tokens)))]
        (m/mset! ret idx (double (count s))))
      ret))
  (add-document [this document]
    (doseq [idx (map (partial index this) (distinct document))]
      (m/mset! dfs idx (inc (m/mget dfs idx))))
    (-> this
        (update-in [:num-docs] (fnil inc 0))
        (update-in [:num-pos] (fnil + 0) (count document))
        (update-in [:num-nnz] (fnil + 0) (count (distinct document)))))
  (add-documents [this documents]    
    (reduce add-document this documents)))

(defn hash-dictionary
  ([hash-size] (HashDictionary. hash-size (m/new-sparse-array hash-size) 0 0 0))
  ([hash-size documents]
   (add-documents (hash-dictionary hash-size) documents)))

(comment
  
  (def document1 (clojure.string/split "A walk in the park" #" "))
  (def document2 (clojure.string/split "I'm all dressed up tonight" #" "))
  (def document3 (clojure.string/split "A walk tonight ?" #" "))
  (def document4 (clojure.string/split "to walk or not to walk" #" "))
 
  (hash-dictionary 30)
  (hash-dictionary 30 [document4])
  (doc2bow (hash-dictionary 30) document4)
  (hash-dictionary 30 [document1 document2 document3 document4])
  (m/non-zero-count (doc2bow (hash-dictionary 30 [document1 document2 document3 document4]) document4))

  (document-frequencies (hash-dictionary 30 [document1 document2 document3]))

  )


