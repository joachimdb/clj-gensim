(ns clj-gensim.corpora.matrix-corpus
  (:require [clj-gensim.protocols :refer :all]
            [clj-gensim.corpora.dictionary :as dict]
            [clj-gensim.corpora.hash-dictionary :as hdict]
            [clojure.core.matrix :as m]))
;; (remove-ns 'clj-gensim.corpora.matrix-corpus)

(defrecord MatrixCorpus [m]
  Corpus
  (num-documents [this] (first (m/shape m)))
  (document-at [this idx] (m/sparse (m/slice m idx)))
  (documents [this]
    (map m/sparse (m/slices m)))
  (add-document [this doc]
    (update-in this [:m] #(m/sparse (m/conjoin %1 %2)) doc)))

(defn matrix-corpus-from-documents
  ([docs]
   (matrix-corpus-from-documents docs {}))
  ([docs opts]
   (assert (coll? docs))
   (assert (satisfies? Document (first docs)))
   (add-documents (MatrixCorpus. (m/new-array [0 (first (m/shape (first docs)))])) docs)))

(defn matrix-corpus-from-texts
  ([texts]
   (matrix-corpus-from-texts texts {}))
  ([texts {:keys [dictionary max-token-index]}]
   (assert (coll? texts))
   (assert (satisfies? TextProtocol (first texts)))
   (when (and dictionary max-token-index)
     (when-not (= max-token-index (.max-token-index dictionary))
       (throw (Exception. (str "conflicting max-token-index: " (.max-token-index dictionary) " =/= " max-token-index)))))
   (let [dict (or dictionary
                  (if max-token-index
                    (hdict/dictionary texts {:max-token-index max-token-index})
                    (dict/dictionary texts)))]
     (matrix-corpus-from-documents (map (partial document dict) texts)))))

(defn matrix-corpus-from-token-counts
  ([token-counts]
   (matrix-corpus-from-token-counts token-counts {}))
  ([token-counts {:keys [max-token-index]}]
   (assert (coll? token-counts))
   (assert (coll? (first token-counts)))
   (let [max-token-index (or max-token-index
                             (inc (reduce (fn [max-idx v] (reduce max max-idx (map first v))) 0 token-counts)))]
     (matrix-corpus-from-documents (map (partial document-from-token-counts max-token-index) token-counts)))))

(defn matrix-corpus
  ([x] (matrix-corpus x {}))
  ([x opts]
   (if (coll? x)
     (if (empty? x)
       (throw (Exception. "Refusing to create empty corpus"))
       (cond (satisfies? Document (first x)) (matrix-corpus-from-documents x opts)
             (satisfies? TextProtocol (first x)) (matrix-corpus-from-texts x opts)
             :else (try (matrix-corpus-from-token-counts x opts)
                        (catch Exception e (str "Don't know how to create matrix corpus from collection of " (type (first x)) "'s")))))
     (throw (Exception. "Don't know how to create matrix corpus from " (type x))))))

(comment

  (matrix-corpus [[[0, 1.0], [1, 1.0], [2, 1.0]],
                  [[2, 1.0], [3, 1.0], [4, 1.0], [5, 1.0], [6, 1.0], [8, 1.0]],
                  [[1, 1.0], [3, 1.0], [4, 1.0], [7, 1.0]],
                  [[0, 1.0], [4, 2.0], [7, 1.0]],
                  [[3, 1.0], [5, 1.0], [6, 1.0]],
                  [[9, 1.0]],
                  [[9, 1.0], [10, 1.0]],
                  [[9, 1.0], [10, 1.0], [11, 1.0]],
                  [[8, 1.0], [10, 1.0], [11, 1.0]]])

  (def texts [{:text "A walk in the park" :language :english}
              {:text "I'm all dressed up tonight" :language "en"}
              {:text "A walk tonight ?" :language "en"}
              {:text "to walk or not to walk" :language "en"}])
  (matrix-corpus texts)

  (matrix-corpus texts {:max-token-index 20000})

  

  )
