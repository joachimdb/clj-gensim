(ns clj-gensim.corpora.hash-dictionary
  (:require [clj-gensim.corpora.dictionary-protocol :refer :all]))

(defn- token-hash [max-hash f]
  (let [r (rem (.hashCode f) max-hash)]
    (if (< r 0)
      (+ r max-hash)
      r)))

(defrecord HashDictionary [max-hash dfs num-docs num-pos num-nnz]
  DictionaryProtocol
  (index [this token] (token-hash max-hash token))
  (doc2bow [this document]
    (reduce (fn [bow id]
              (update-in bow [id] (fnil inc 0)))
            (sorted-map)
            (map (partial index this) document)))
  (add-document [this document]
    (reduce #(update-in %1 [:dfs %2] (fnil inc 0))
            (-> this
                (update-in [:num-docs] (fnil inc 0))
                (update-in [:num-pos] (fnil + 0) (count document))
                (update-in [:num-nnz] (fnil + 0) (count (distinct document))))
            (map (partial index this) (distinct document))))
  (add-documents [this documents]    
    (reduce add-document this documents)))

(defn hash-dictionary
  ([] (HashDictionary. 200000 {} 0 0 0))
  ([documents]
   (add-documents (hash-dictionary) documents)))

(comment
  
  (def document1 (clojure.string/split "A walk in the park" #" "))
  (def document2 (clojure.string/split "I'm all dressed up tonight" #" "))
  (def document3 (clojure.string/split "A walk tonight ?" #" "))
  (def document4 (clojure.string/split "to walk or not to walk" #" "))
 
  (hash-dictionary)
  (doc2bow (hash-dictionary) document4)
  (add-document (hash-dictionary) document4)
  (add-document (add-document (hash-dictionary) document1) document4)

  (add-documents (hash-dictionary) [document1 document3])
  
  (hashing-dictionary [document1 document2 document3 document4])  

  )


