(ns clj-gensim.corpora.dictionary)
;; (remove-ns 'clj-gensim.corpora.dictionary)

(defprotocol DictionaryProtocol
  (doc2bow [this document])
  (add-document [this document])
  (add-documents [this documents] [this documents prune-at]))

(defrecord Dictionary [token2id dfs num-docs num-pos num-nnz]
  DictionaryProtocol
  (doc2bow [this document]
    (reduce (fn [bow id]
              (update-in bow [id] (fnil inc 0)))
            (sorted-map)
            (filter identity (map token2id document))))
  (add-document [this document]
    (reduce (fn [this w]
              (let [id (get (:token2id this) w (count (:token2id this)))]
                (-> this
                    (assoc-in [:token2id w] id)
                    (update-in [:dfs id] (fnil inc 0)))))
            (-> this
                (update-in [:num-docs] (fnil inc 0))
                (update-in [:num-pos] (fnil + 0) (count document))
                (update-in [:num-nnz] (fnil + 0) (count (distinct document))))
            (distinct document)))
  (add-documents [this documents]
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
  ([] (Dictionary. {} {} 0 0 0))
  ([documents]
   (add-documents (dictionary) documents 2000000)))

(comment

  (dictionary)

  (def document1 (clojure.string/split "A walk in the park" #" "))
  (def document2 (clojure.string/split "I'm all dressed up tonight" #" "))
  (def document3 (clojure.string/split "A walk tonight ?" #" "))
  
  (doc2bow (dictionary) document1)
  (add-document (dictionary) document1)
  (add-document (add-document (dictionary) document1) document3)

  (add-documents (dictionary) [document1 document3])
  
  (dictionary [document1 document2 document3])  

  )


