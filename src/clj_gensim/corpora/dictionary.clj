(ns clj-gensim.corpora.dictionary
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.dictionary)

(defrecord Dictionary [token2id dfs num-docs num-pos num-nnz]
  DictionaryProtocol
  (index [this token] (token2id token))
  (document-frequencies [this] dfs)
  (num-documents [this] num-docs)
  (num-tokens [this] num-pos)
  (num-unique-tokens [this] num-nnz)
  (doc2bow [this tokens]
    (let [ret (m/new-sparse-array (count token2id))]
      (doseq [[idx s] (group-by identity (remove nil? (map #(index this %) tokens)))]
        (m/mset! ret idx (double (count s))))
      ret))
  (add-document [this document]
    (reduce (fn [this w]
              (let [idx (get (:token2id this) w (count (:token2id this)))
                    new-dfs (m/reshape (:dfs this) [(inc (max idx (count (:token2id this))))])]
                (m/mset! new-dfs idx (inc (m/mget new-dfs idx)))
                (-> this
                    (assoc-in [:token2id w] idx)
                    (assoc-in [:dfs] new-dfs))))
            (-> this
                (update-in [:num-docs] (fnil inc 0))
                (update-in [:num-pos] (fnil + 0) (count document))
                (update-in [:num-nnz] (fnil + 0) (count (distinct document))))
            (distinct document)))
  (add-documents [this documents]pr
    (add-documents this documents 2000000))
  (add-documents [this documents prune-at]    
    (reduce (fn [this [docno document]]
              (when (and prune-at
                         (> (count token2id) prune-at))
                (throw (Exception. "not implemented")))
              (add-document this document))
            this
            (map vector (range) documents))))

(defn dictionary
  ([] (Dictionary. {} (m/new-array [0]) 0 0 0))
  ([documents]
   (add-documents (dictionary) documents)))

(comment

  (def document1 (clojure.string/split "A walk in the park" #" "))
  (def document2 (clojure.string/split "I'm all dressed up tonight" #" "))
  (def document3 (clojure.string/split "A walk tonight ?" #" "))
  (def document4 (clojure.string/split "to walk or not to walk" #" "))
  (def document4 (clojure.string/split "to walk in the park or not to walk in the park" #" "))

  (add-document (dictionary) document4)
  
  (def tokens document1)
  (doc2bow (dictionary) document1)
  (add-document (dictionary) document1)
  (doc2bow (add-document (dictionary) document1) document1)
  (doc2bow (dictionary [document1 document2 document3 document4]) document1)
  (doc2bow (dictionary [document1 document2 document3 document4]) document4)
  
  (add-document (add-document (dictionary) document1) document3)

  (add-documents (dictionary) [document1 document3])
  
  (document-frequencies (dictionary [document1 document2 document3]))

  )
