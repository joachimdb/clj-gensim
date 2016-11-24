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
  (transform* [this v] (m/mul v idfs))
  (transform*! [this v] (m/mul! v idfs)))


(defn tfidf-model
  ([x] (tfidf-model x {}))
  ([x {:keys [add] :or {add 0.0}}]
   (cond (satisfies? DictionaryProtocol x)
         (TfIdfModel. (compute-idfs-from-dfs (document-frequencies x) (num-documents x) add))
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


