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

(defn- compute-idfs-from-documents [documents add]
  (let [dfs (m/set-selection (first documents) (m/non-zero-indices (first documents)) 1.0)
        totaldocs (reduce (fn [num-doc doc]
                            (m/add! dfs (m/set-selection doc (m/non-zero-indices doc) 1.0))
                            (inc num-doc))
                          1
                          (rest documents))]
    (compute-idfs-from-dfs dfs totaldocs add)))

(defrecord TfIdfModel [idfs add]
  Transformation
  (transform* [this v] (m/mul v idfs))
  (transform*! [this v] (m/mul! v idfs))
  Model
  (train* [this corpus]
    (if (nil? idfs)
      (assoc this :idfs (compute-idfs-from-documents (documents corpus) add))
      (throw (Exception. "not implemented")))))


(defn tfidf-model
  ([] (tfidf-model nil {}))
  ([x] (tfidf-model x {}))
  ([x {:keys [add] :or {add 0.0} :as opts}]
   (cond (nil? x)
         (TfIdfModel. nil add)
         (satisfies? DictionaryProtocol x)
         (TfIdfModel. (compute-idfs-from-dfs (document-frequencies x) (num-documents x) add) add)
         (satisfies? Corpus x)
         (train* (TfIdfModel. nil add) x)
         :else (throw (Exception. (str "Don't know how to create TfIdfModel from " (type x)))))))

(comment

  (use 'clj-gensim.corpora.dictionary)
  ;; (use 'clj-gensim.corpora.hash-dictionary)

  (def dict (dictionary [document1 document2 document3 document4]))
  ;; (def dict (hash-dictionary 2000 [document1 document2 document3 document4]))

  (def tfidf (tfidf-model dict))

  (transform tfidf (document dict document4))
  (transform tfidf (map (partial document dict)
                        [document1 document2 document3 document4]))
  
  )


