(ns clj-gensim.protocols
  (:refer-clojure :exclude [load vec empty])
  (require [clojure.tools.logging :as log]
           [clojure.core.matrix :as m]
           [clojure.java.io :as io]
           [clj-gensim.analysis.lucene :refer :all])
  (import [java.io ObjectInputStream ObjectOutputStream]))
;; (remove-ns 'clj-gensim.protocols)
(m/set-current-implementation :vectorz)

(defprotocol SaveLoad
  (save* [this destination opts] "opts include :append and :encoding")
  (load* [this source opts]))

(defn save-object [x destination opts]
  (with-open [os (ObjectOutputStream. (io/make-output-stream destination opts))]
    (.writeObject os x)))

(defn load-object [source opts]
  (with-open [is (ObjectInputStream. (io/make-input-stream source opts))]
     (.readObject is)))

(defn save
  ([object destination] (save object destination {}))
  ([object destination opts]
   (save* object destination opts)))

(defn load
  ([source]
   (load source {}))
  ([source opts]
   (load-object source opts))
  ([object source opts]
   (load* object source opts)))

(extend-protocol SaveLoad
  java.io.Serializable
  (save* [this destination opts]
    (save-object this destination opts))
  (load* [this source opts]
    (let [loaded (load-object source opts)]
      (if (= (type loaded) (type this))
        loaded
        (throw (Exception. (str "Object loadef from \"" source "\" is of type " (type loaded) ", expected " (type this))))))))

;;; Text
;;; ====

(defprotocol TextProtocol
  (text [this] [this default])
  (language [this] [this default])
  (tokens [this]))
(defn text? [x] (satisfies? TextProtocol x))
(extend-protocol TextProtocol
  clojure.lang.Keyword
  (language
    ([this] (or (KnownLanguages this) (throw (Exception. (str "unknown language: " this)))))
    ([this default] (or (KnownLanguages this) (KnownLanguages default) (throw (Exception. (str "unknown language: " default))))))
  java.lang.String
  (text
    ([this] this)
    ([this default] this))
  (language
    ([this] (or (KnownLanguages this) (throw (Exception. (str "unknown language: " this)))))
    ([this default] (or (KnownLanguages this) (KnownLanguages default) (throw (Exception. (str "unknown language: " default))))))
  clojure.lang.IPersistentMap
  (text
    ([this] (or (text (:text this)) (throw (Exception. (str "unknown text: " this)))))
    ([this default] (or (text (:text this))
                        (text (:default this))()
                        (throw (Exception. (str "unknown texts: " this ", " default))))))
  (language
    ([this] (or (language (:language this)) (throw (Exception. (str "unknown language: " this)))))
    ([this default] (or (language (:language this))
                        (language (:default this))()
                        (throw (Exception. (str "unknown languages: " this ", " default))))))
  (tokens [this] (map :char-term (analyze (text-analyzer (language this)) (text this) #{:char-term}))))

;;; Dictionary (a mapping from tokens to indices)
;;; =============================================

(defprotocol DictionaryProtocol
  (document-frequencies [this])
  (token-index [this token])
  (max-token-index [this])
  (add-tokens [this tokens]))
(defn dictionary? [x]
  (satisfies? DictionaryProtocol x))

(defn add-text [dictionary text]
  (add-tokens dictionary (tokens text)))
(defn add-texts [dictionary texts]
  (reduce add-text dictionary texts))

(defn token-counts [dictionary text]
  (frequencies (remove nil? (map (partial token-index dictionary) (tokens text)))))

;;; Document (A document is basically a vector)
;;; ===========================================

(defprotocol DocumentProtocol
  (length [this]))
(defn document? [x]
  (satisfies? DocumentProtocol x))
(extend-protocol DocumentProtocol
  mikera.vectorz.IVector
  (length [this] (first (m/shape this))))

(defn document-from-token-counts
  ([token-counts]
   (document-from-token-counts (inc (reduce max (map first token-counts))) token-counts))
  ([max-token-index token-counts]
   (let [v (m/new-sparse-array max-token-index)]
     (doseq [[idx cnt] token-counts]
       (m/mset! v idx (double cnt)))
     v)))

;;; DocumentBatch (A matrix with documents as rows
;;; ==============================================

(defprotocol DocumentBatchProtocol)
(defn batch? [x]
  (satisfies? DocumentBatchProtocol x))
(extend-protocol DocumentBatchProtocol
  mikera.matrixx.IMatrix)

(defn- empty-batch [max-token-index]
  (m/new-sparse-array [0 max-token-index]))

(defn batch
  ([docs]
   (batch docs {}))
  ([docs opts]
   (assert (coll? docs))
   (assert (satisfies? DocumentProtocol (first docs)))
   (reduce #(m/sparse (m/conjoin %1 %2))
           (empty-batch (length (first docs)))
           docs)))

;;; Corpus (a collection of documents)
;;; ==================================

(defprotocol CorpusProtocol
  (num-documents [this])
  (add-document [this doc])
  (document-at [this idx])
  (documents [this])
  (empty [this]))
(defn corpus? [x]
  (satisfies? CorpusProtocol x))
(extend-protocol CorpusProtocol
  clojure.lang.ISeq
  (num-documents [this] (count this))
  (documents [this] this)
  (empty [this] '())
  mikera.matrixx.IMatrix
  (num-documents [this] (m/row-count this))
  (add-document [this doc] (m/sparse (m/conjoin this doc)))
  (document-at [this idx] (m/select this idx :all))
  (documents [this] (seq (m/rows this)))
  (empty [this] (empty-batch (m/column-count this))))

(defn add-documents [into-corpus corpus]
  (assert (satisfies? CorpusProtocol into-corpus))
  (assert (satisfies? CorpusProtocol corpus))
  (reduce add-document into-corpus (documents corpus)))

(defn corpus-from-token-counts
  ([token-counts]
   (corpus-from-token-counts token-counts {}))
  ([token-counts {:keys [max-token-index] :as opts}]
   (assert (coll? token-counts))
   (assert (coll? (first token-counts)))
   (let [max-token-index (or max-token-index
                             (inc (reduce (fn [max-idx v] (reduce max max-idx (map first v))) 0 token-counts)))]
     (batch (map (partial document-from-token-counts max-token-index) token-counts) opts))))

(defn corpus-from-texts
  ([dictionary texts]
   (corpus-from-texts dictionary texts {}))
  ([dictionary texts opts]
   (assert (dictionary? dictionary))
   (assert (coll? texts))
   (assert (text? (first texts)))
   (corpus-from-token-counts (map (partial token-counts dictionary) texts))))

(defn corpus
  ([docs]
   (if (corpus? docs)
     docs
     (corpus docs {})))
  ([docs opts]
   (if (corpus? docs)
     docs
     (do (assert (sequential? docs))
         (cond (empty? docs) '()
               (document? (first docs)) (batch docs)
               :else (corpus-from-token-counts docs opts)))))
  ([dictionary texts opts]
   (corpus-from-texts dictionary texts opts)))

;;; From Text to Document
;;; =====================

(defn document [x t]
  (cond (and (dictionary? x) (text? t))
        (document-from-token-counts (max-token-index x) (token-counts x t))
        (batch? x)
        (document-from-token-counts (m/column-count x) t)
        (corpus? x)
        (document-from-token-counts (length (first (documents x))) t)))

;;; Models
;;; ======

(defprotocol ModelProtocol
  (transform-document [this doc])
  (transform-batch [this batch])
  (train-document [this doc])
  (train-batch [this corpus]))

(defn transform [model x]
  (assert (satisfies? ModelProtocol model))
  (cond (document? x) (transform-document model x)
        (batch? x) (try (transform-batch model x)
                        (catch Exception e (transform model (documents x))))
        (seq? x) (map (partial transform-document model)
                      (documents x))
        (corpus? x) (add-documents (empty x) ;;; THIS SHOULD BE THE SAME AS (MAP ...) IN CASE THAT X IS A SEQUENCE, BUT IT IS NOT, Hence the seq? clause above
                                   (map (partial transform-document model)
                                        (documents x)))
        :else (throw (Exception. (str "Don't know how to transform object of type " (type x))))))

(defn train [model x]
  (assert (satisfies? ModelProtocol model))
  (cond (document? x) (train-document model x)
        (batch? x) (try (train-batch model x)
                        (catch Exception e (train model (documents x))))
        (corpus? x) (reduce train-document model (documents x))
        :else (throw (Exception. (str "Don't know how to transform object of type " (type x))))))


;;; Similarities
;;; ============

(defprotocol SimilarityProtocol
  (sim-document [this doc])
  (sim-batch [this batch]))

(defn sim [s x]
  (assert (satisfies? SimilarityProtocol s))
  (cond (document? x) (sim-document s x)
        (batch? x) (try (sim-batch s x)
                        (catch Exception e (sim s (documents x))))
        (corpus? x) (reduce sim-document s (documents x))
        :else (throw (Exception. (str "Don't know how to compute similarity to object of type " (type x))))))


(comment
  
  ;;; SaveLoad tests

  (save (m/matrix [[1 2] [3 4]]) "/tmp/testm")
  (load "/tmp/testm")

  (save (m/new-sparse-array 10) "/tmp/testm")
  (load "/tmp/testm")

  (save [(m/matrix [[1 2] [3 4]]) (m/new-sparse-array 10) ] "/tmp/testm")
  (load "/tmp/testm")

  ;;; Text, language and tokens test 

  (language :english)
  (language "en")
  (language "english") ;; throws exception
  (language {:language "en"})
  
  (text "Het kruim van de Gentse techscene verzamelt vandaag voor de eerste van meerdere ‘Level Up Sessions’.")
  (text {:text "Het kruim van de Gentse techscene verzamelt vandaag voor de eerste van meerdere ‘Level Up Sessions’."})

  (tokens {:text "Het kruim van de Gentse techscene verzamelt vandaag voor de eerste van meerdere ‘Level Up Sessions’."
           :language "nl"})

  )
