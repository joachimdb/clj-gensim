(ns clj-gensim.corpora.dictionary-protocol)

(defprotocol DictionaryProtocol
  (index [this token])
  (doc2bow [this document])
  (add-document [this document])
  (add-documents [this documents] [this documents prune-at]))
