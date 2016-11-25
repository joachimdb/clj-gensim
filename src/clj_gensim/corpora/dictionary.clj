(ns clj-gensim.corpora.dictionary
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.dictionary)

(defrecord Dictionary [token2id dfs num-docs num-pos num-nnz]
  Corpus
  (num-documents [this] num-docs)
  DictionaryProtocol
  (token-index [this token] (token2id token))
  (max-token-index [this] (count token2id))
  (document-frequencies [this] dfs)
  (add-tokens [this tokens]
    (reduce (fn [this w]
              (let [current-size (max-token-index this)
                    idx (get (:token2id this) w current-size)
                    new-dfs (m/reshape (:dfs this) [(max (inc idx) current-size)])]
                (m/mset! new-dfs idx (inc (m/mget new-dfs idx)))
                (-> this
                    (assoc-in [:token2id w] idx)
                    (assoc-in [:dfs] new-dfs))))
            (-> this
                (update-in [:num-docs] (fnil inc 0))
                (update-in [:num-pos] (fnil + 0) (count tokens))
                (update-in [:num-nnz] (fnil + 0) (count (distinct tokens))))
            (distinct tokens)))
  DocumentSource
  (document [this x]
    (assert (satisfies? TextProtocol x))
    (document-from-token-counts (max-token-index this) (frequencies (remove nil? (map #(token-index this %) (tokens x)))))))

(defn dictionary
  ([] (dictionary nil {}))
  ([texts] (dictionary texts {}))
  ([texts {}]
   (let [dict (Dictionary. {} (m/new-array [0]) 0 0 0)]
     (if (empty? texts)
       dict
       (add-texts dict texts)))))

(comment

  (def document1 {:text "A walk in the park" :language :english})
  (def document2 {:text "I'm all dressed up tonight" :language "en"})
  (def document3 {:text "A walk tonight ?" :language "en"})
  (def document4 {:text "to walk or not to walk" :language "en"})

  (tokens document4)
  (add-text (dictionary) document4)
  (max-token-index (add-text (dictionary) document4))

  (tokens document1)
  (def d (add-text (dictionary) document1))
  (max-token-index d)
  (document d document1)
  (document (add-text d document1) document1)
  (document (dictionary [document1 document2 document3 document4]) document1)
  (document (dictionary [document1 document2 document3 document4]) document4)
  
  (document-frequencies (dictionary [document1 document2 document3]))
  
  )
