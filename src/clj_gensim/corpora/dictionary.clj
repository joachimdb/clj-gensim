(ns clj-gensim.corpora.dictionary
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.dictionary)

(defrecord Dictionary [token2id dfs num-docs num-pos num-nnz]
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
            (distinct tokens))))

(defn dictionary
  ([] (dictionary nil {}))
  ([texts] (dictionary texts {}))
  ([texts {}]
   (let [dict (Dictionary. {} (m/new-array [0]) 0 0 0)]
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
