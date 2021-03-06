(ns clj-gensim.corpora.sharded-corpus
  (:refer-clojure :exclude [load empty])  
  (:require [clj-gensim.protocols :refer :all]
            [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))
;; (remove-ns 'clj-gensim.corpora.sharded-corpus)

(defprotocol CorpusShardProtocol
  (open [this])
  (close [this])
  (open? [this])
  (persist [this])
  (location [this])
  (offset [this]))
(defn shard? [x]
  (satisfies? CorpusShardProtocol x))

(defn contains-document? [shard idx]
  (let [shard-idx (- idx (offset shard))] 
    (and (>= shard-idx 0)
         (< shard-idx (num-documents shard)))))

(defn save-shard [shard destination opts]
  (if (= (io/as-file destination) (location shard))
    (do (io/make-parents destination)
        (save-object shard destination opts))
    (throw (Exception. (str "Attempting to save shard at location '" (location shard) "' to '" destination "'"))))
  true)

(defn load-shard [shard source opts]
  (let [l (load-object source opts)]
    (if (shard? l)
      (if (= (location l) (location shard))
        (if (= (offset l) (offset shard))
          (assoc shard :batch (:batch l))
          (throw (Exception. (str "Attempt to load shard with offset '" (offset l) "' into shard with offset '" (offset shard) "'"))))
        (throw (Exception. (str "Attempt to load shard at location '" (location shard) "' from shard at location'" (location l) "'"))))
      (throw (Exception. (str "Attempt to load object of type " (type l) " into object of type ShardP"))))))

(defrecord ShardBatch [batch offset num-doc loc changed?]
  DocumentBatchProtocol
  SaveLoad
  (save* [this destination opts]
    (if (save-shard this (io/as-file destination) opts)
      (assoc this :changed? false)
      (throw (Exception. (str "Failed to save shard to " destination)))))
  (load* [this source opts]
         (assoc (load-shard this loc opts) :changed? false))
  CorpusProtocol
  (num-documents [this] num-doc)
  (add-document [this doc]
    (if (nil? batch)
      (throw (Exception. "Attempt to add documents to closed shard"))
      (-> this
          (update-in [:batch] add-document doc)
          (update-in [:num-doc] inc)
          (assoc :changed? true))))
  (document-at [this idx]
    (if (nil? batch)
      (throw (Exception. "Attempt to access a document from closed shard"))
      (let [shard-idx (- idx offset)] 
        (if (or (< shard-idx 0)
                (>= shard-idx num-doc))
          (throw (Exception. (str "Attempt to access a document at position " idx ", which is outside the shard range [" offset ", " (+ offset num-doc) "[")))
          (document-at batch shard-idx)))))
  (documents [this]
    (if (nil? batch)
      (throw (Exception. "Attempt to access documents from closed shard"))
      (documents batch)))
  CorpusShardProtocol
  (open? [this] (not (nil? batch)))
  (open [this]
    (if (open? this)
      this
      (load this loc {})))
  (close [this]
    (if (open? this)
      (if (or (not changed?)
              (save-shard this loc {}))
        (assoc this :batch nil :changed? false)
        (throw (Exception. "Could not save shard before close")))
      this))
  (persist [this]
    (when changed?
      (save this loc {}))
    true)
  (location [this] loc)
  (offset [this] offset))

(defn shard [docs offset location]
  (when (.exists (io/as-file location))
    (throw (Exception. (str "Attempt to create matrix-corpus-shard at existing location '" location "'"))))
  (let [b (batch docs)
        ret (ShardBatch. b offset (num-documents b) (io/as-file location) true)]
    (or (save ret location)
        (throw (Exception. (str "Failed to create shard at location '" location "'"))))))

(defn shard-at [shards idx]
  (first (filter #(contains-document? % idx) shards)))

(defn- open-shard [shards shard]
  (if (open? shard)
    shards
    (conj (doall (map close (filter #(not= shard %) shards))) 
          (open shard))))

(defn- close-shards [shards]
  (println "closing shards")
  (doall (map close shards)))

(defn- open-and-add [shards shard doc]
  (conj (doall (map close (filter #(not= shard %) shards))) 
        (add-document (open shard) doc)))

(defn- open-shard-at [shards idx]
  (open-shard shards (shard-at shards idx)))

(defrecord ShardedCorpus [max-shard-size shards loc]
  CorpusProtocol
  (num-documents [this] (reduce + 0 (map num-documents @shards)))
  (add-document [this doc]
    (let [last-shard (first (sort-by offset > @shards))]
      (if (or (nil? last-shard)
              (>= (num-documents last-shard) max-shard-size))
        (let [offset (if last-shard
                       (+ (offset last-shard) (num-documents last-shard))
                       0)]
          (println "Starting new shard at offset" offset)
          (update-in this [:shards] swap!
                     #(conj (close-shards %)
                            (shard [doc]
                                   offset
                                   (str loc "shard." offset)))))
        (update-in this [:shards] swap!
                   open-and-add last-shard doc)))
    this)
  (document-at [this idx]
    (swap! shards #(open-shard-at % idx))
    (document-at (shard-at @shards idx) idx))
  (documents [this]
    (mapcat (fn [idx]
              (swap! shards #(open-shard-at % idx))
              (documents (shard-at @shards idx)))
            (sort (map offset @shards))))
  SaveLoad
  (save* [this destination opts]
    (update-in this [:shards] swap! close-shards)
    (save-object (update-in this [:shards] deref) (str loc "__ShardedCorpus") opts)
    this)
  (load* [this source opts]
    (update-in (load-object (str source "__ShardedCorpus") opts) [:shards] atom)))

(defn sharded-corpus
  ([] (ShardedCorpus. nil (atom '()) nil))
  ([max-shard-size loc docs]
   (add-documents (ShardedCorpus. max-shard-size
                                  (atom '())
                                  loc)
                  (corpus docs))))

(comment
  
  (use 'clj-gensim.corpora.dictionary)

  (def texts [{:text "A walk in the park" :language :english}
              {:text "I'm all dressed up tonight" :language "en"}
              {:text "A walk tonight ?" :language "en"}
              {:text "to walk or not to walk" :language "en"}])
  (def dict (dictionary texts))
  (def docs (map (partial document dict) texts))
  (def doc (first docs))
  
  (def max-shard-size 20000)
  (def loc "/tmp/test-corpus/")
  (def c (sharded-corpus 20000 loc []))
  (def ic (take 100000 (cycle docs)))
  (time (def c (add-documents c ic)))
  "Elapsed time: 65635.294536 msecs"
  ;; takes 7.3M on disk (14880 blocks)
  
  (num-documents c)
  (time (count (documents c)))
  "Elapsed time: 87.639925 msecs"
  ;; That's more than 1M documents per second!

  (use 'clj-gensim.corpora.hash-dictionary)

  (def dict (hash-dictionary 20000 [document1 document2 document3 document4]))
  (def docs (map (partial document dict)
                 [document1 document2 document3 document4]))

  (def loc "/tmp/test-corpus-sparse/")
  (def c (sharded-corpus 20000 loc []))
  (time (def c (add-documents c (take 100000 (cycle docs)))))
  "Elapsed time: 24233.033588 msecs"
  ;; takes 0.5M on disk (1000 blocks)

  ;; that's almost 3 times faster and only uses about 1/15th of space 

  (num-documents c)
  (time (count (documents c)))
  "Elapsed time: 14.759626 msecs"
  ;; That's almost than 8M documents per second!



  
  (save c "/tmp/test-corpus/")
  (def c (load (sharded-corpus) "/tmp/test-corpus/" {}))

  )

