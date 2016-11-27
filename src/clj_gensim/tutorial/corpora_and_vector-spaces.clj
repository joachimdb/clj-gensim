(ns clj-gensim.tutorial.corpora-and-vector-spaces
  (:refer-clojure :exclude [load])
  (:require [clojure.core.matrix :as m]           
            [clojure.java.io :as io]
            [clj-gensim.protocols :refer :all]
            [clj-gensim.corpora.dictionary :refer (dictionary)]
            [clj-gensim.models.tfidf-model :refer (tfidf-model)]
            ))


(def docs [{:language "en" :text "Human machine interface for lab abc computer applications"},
           {:language "en" :text "A survey of user opinion of computer system response time"},
           {:language "en" :text "The EPS user interface management system"},
           {:language "en" :text "System and human system engineering testing of EPS"},
           {:language "en" :text "Relation of user perceived response time to error measurement"},
           {:language "en" :text "The generation of random binary unordered trees"},
           {:language "en" :text "The intersection graph of paths in trees"},
           {:language "en" :text "Graph minors IV Widths of trees and well quasi ordering"},
           {:language "en" :text "Graph minors A survey"}])
(map tokens docs)

(def dict (dictionary docs))

(:token2id dict)

(document dict {:text "Human computer interaction" :language "en"})

(def c (corpus dict docs {}))

;; TODO: corpus streaming

