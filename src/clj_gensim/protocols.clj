(ns clj-gensim.protocols
  (:refer-clojure :exclude [load])
  (require [clojure.core.matrix :as m]
           [clojure.java.io :as io]
           [clj-gensim.analysis.lucene :refer :all])
  (import [java.io ObjectInputStream ObjectOutputStream]))
;; (remove-ns 'clj-gensim.protocols)
(m/set-current-implementation :vectorz)

(defprotocol SaveLoad
  (save* [this destination opts] "opts include :append and :endocing")
  (load* [this source opts]))

(defn save
  ([object destination] (save object destination {}))
  ([object destination opts]
   (save* object destination opts)))

(defn load
  ([source]
   (load source {}))
  ([source opts]
   (with-open [is (ObjectInputStream. (io/make-input-stream source opts))]
     (.readObject is)))
  ([object source opts]
   (load* object source opts)))

(extend-protocol SaveLoad
  java.io.Serializable
  (save* [this destination opts]
    (with-open [os (ObjectOutputStream. (io/make-output-stream destination opts))]
      (.writeObject os this)))
  (load* [this source opts]
    (let [loaded (load source opts)]
      (if (= (type loaded) (type this))
        loaded
        (throw (Exception. (str "Object loadef from \"" source "\" is of type " (type loaded) ", expected " (type this))))))))

;;; Plain text

(defprotocol TextProtocol
  (text [this] [this default])
  (language [this] [this default])
  (tokens [this]))

(extend-protocol TextProtocol
  java.lang.String
  (text
    ([this] this)
    ([this default] this))
  (language
    ([this] (or (KnownLanguages this) (throw (Exception. (str "unknown language: " this)))))
    ([this default] (or (KnownLanguages this) (KnownLanguages default) (throw (Exception. (str "unknown language: " default))))))
  java.lang.Object
  (language
    ([this] (or (KnownLanguages this) (throw (Exception. (str "unknown language: " this)))))
    ([this default] (or (KnownLanguages this) (KnownLanguages default) (throw (Exception. (str "unknown language: " default))))))
  (tokens [this] (map :char-term (analyze (text-analyzer (language this)) (text this) #{:char-term})))
  clojure.lang.Associative
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


;;; Document, Corpus and Dictionary

(defprotocol DictionaryProtocol
  (document-frequencies [this])
  (token-index [this token])
  (max-token-index [this])
  (add-tokens [this tokens]))

(defn add-text [dictionary text]
  (add-tokens dictionary (tokens text)))

(defn add-texts [dictionary texts]    
  (reduce add-text dictionary texts))

(defprotocol Document)

(extend-protocol Document
  mikera.arrayz.impl.AbstractArray
  mikera.vectorz.AVector
  mikera.arrayz.INDArray)

(defn document? [x]
  (satisfies? Document x))

(defprotocol DocumentSource
  (document [this x]))

(defprotocol Corpus
  (num-tokens [this])
  (num-nonzero [this])
  (num-documents [this])
  (add-document [this doc])
  (document-at [this idx])
  (documents [this])
  (document-matrix [this]))

;;; TODO: remove document-matrix from Corpus protocol

(defn add-documents [this documents]
  (reduce add-document this documents))

(defprotocol Transformation
  (transform* [this v])
  (transform*! [this v]))

(defn transform [t x]
  (cond (seq? x) (if (empty? x)
                   x
                   (cons (transform t (first x))
                         (lazy-seq (transform t (rest x)))))
        (m/vec? x) (transform* t x)
        :else (throw (Exception. (str "Don't know how to transform " (type x)))))  )

(defn transform! [t x]
  (cond (seq? x) (if (empty? x)
                   x
                   (cons (transform! t (first x))
                         (lazy-seq (transform! t (rest x)))))
        (m/vec? x) (transform*! t x)
        :else (throw (Exception. (str "Don't know how to transform " (type x)))))  )


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
