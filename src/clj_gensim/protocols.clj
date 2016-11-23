(ns clj-gensim.protocols)

(defprotocol DictionaryProtocol
  (document-frequencies [this])
  (index [this token])
  (num-documents [this])
  (num-tokens [this])
  (num-unique-tokens [this])
  (doc2bow [this document])
  (add-document [this document])
  (add-documents [this documents] [this documents prune-at]))

(defprotocol Transformation
  (init [this s] [this s opts])
  (transform [this s]))

