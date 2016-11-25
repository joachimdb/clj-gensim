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
  Corpus
  (num-documents [this] num-docs)
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
        (update-in [:num-nnz] (fnil + 0) (count (distinct tokens)))))
  DocumentSource
  (document [this x]
    (assert (satisfies? TextProtocol x))
    (document-from-token-counts (max-token-index this) (frequencies (remove nil? (map #(token-index this %) (tokens x)))))))

(defn dictionary
  ([] (dictionary nil {:max-token-index 20000}))
  ([texts] (dictionary texts {:max-token-index 20000}))
  ([texts {:keys [max-token-index]}]
   (let [dict (HashDictionary. max-token-index (m/new-sparse-array max-token-index) 0 0 0)]
     (if (empty? texts)
       dict
     (add-texts dict texts)))))

(comment

  (def document1 {:text "A walk in the park" :language :english})
  (def document2 {:text "I'm all dressed up tonight" :language "en"})
  (def document3 {:text "A walk tonight ?" :language "en"})
  (def document4 {:text "to walk or not to walk" :language "en"})

  (tokens document4)
  (add-text (hash-dictionary 20) document4)
  (max-token-index (add-text (hash-dictionary 20) document4))

  (tokens document1)
  (def d (add-text (dictionary 20) document1))
  (max-token-index d)
  (document d document1)
  (document (add-text d document1) document1)
  (document (dictionary 20 [document1 document2 document3 document4]) document1)
  (document (dictionary 20 [document1 document2 document3 document4]) document4)
  
  (document-frequencies (hash-dictionary 20 [document1 document2 document3]))
  
  )


