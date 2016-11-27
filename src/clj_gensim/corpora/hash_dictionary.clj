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
  (token-index [this token] (token-hash max-hash token))
  (max-token-index [this] max-hash)
  (document-frequencies [this] dfs)
  (add-tokens [this tokens]
    (doseq [idx (map (partial token-index this) (distinct tokens))]
      (m/mset! dfs idx (inc (m/mget dfs idx))))
    (-> this
        (update-in [:num-docs] (fnil inc 0))
        (update-in [:num-pos] (fnil + 0) (count tokens))
        (update-in [:num-nnz] (fnil + 0) (count (distinct tokens))))))

(defn dictionary
  ([] (dictionary nil {:max-token-index 20000}))
  ([texts] (dictionary texts {:max-token-index 20000}))
  ([texts {:keys [max-token-index]}]
   (let [dict (HashDictionary. max-token-index (m/new-sparse-array max-token-index) 0 0 0)]
     (if (empty? texts)
       dict
     (add-texts dict texts)))))


(comment

  (def texts [{:text "A walk in the park" :language :english}
              {:text "I'm all dressed up tonight" :language "en"}
              {:text "A walk tonight ?" :language "en"}
              {:text "to walk or not to walk" :language "en"}])

  (tokens (last texts))
  (add-text (dictionary) (last texts))
  (max-token-index (add-text (dictionary) (last texts)))

  (tokens (first texts))
  (def d (add-text (dictionary) (first texts)))
  (max-token-index d)
  (document d (first texts))
  (document (add-text d (first texts)) (first texts))
  (document (dictionary texts) (first texts))
  (document (dictionary texts) (last texts))
  
  (document-frequencies (dictionary texts))
  
  )



