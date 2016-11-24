(ns clj-gensim.analysis.lucene
  (require [clojure.java.io :as io])
  (:import (java.io Reader StringReader)
           (org.apache.lucene.analysis Analyzer TokenStream)
           (org.apache.lucene.analysis.charfilter NormalizeCharMap)
           (org.apache.lucene.analysis.charfilter MappingCharFilter HTMLStripCharFilter)
           (org.apache.lucene.analysis.pattern PatternReplaceCharFilter)
           (org.apache.lucene.analysis.standard StandardFilter StandardTokenizer StandardAnalyzer)
           (org.apache.lucene.analysis.miscellaneous ASCIIFoldingFilter LengthFilter)
           (org.apache.lucene.analysis.ngram EdgeNGramTokenizer)
           (org.apache.lucene.util AttributeSource)
           (org.apache.lucene.analysis.tokenattributes CharTermAttribute OffsetAttribute)
           (org.apache.lucene.analysis.core StopFilter)
           (org.apache.lucene.analysis.fr FrenchAnalyzer)
           (org.apache.lucene.analysis.nl DutchAnalyzer)
           (org.apache.lucene.analysis.it ItalianAnalyzer)
           (org.apache.lucene.analysis.es SpanishAnalyzer)
           (org.apache.lucene.analysis.tr TurkishAnalyzer)
           (org.apache.lucene.analysis.de GermanAnalyzer)
           (org.apache.lucene.analysis.en EnglishAnalyzer)
           (org.apache.lucene.analysis.ar ArabicAnalyzer)
           ))
;; (remove-ns 'clj-gensim.corpora.text-document.analysis)

(defonce English :english)
(defonce German :german)
(defonce French :french)
(defonce Dutch :dutch)
(defonce Italian :italian)
(defonce Spanish :spanish)
(defonce Turkish :turkish)
(defonce Arabic :arabic)

(def KnownLanguages
  {"en" English
   "de" German
   "fr" French
   "nl" Dutch
   "it" Italian
   "es" Spanish
   "tr" Turkish
   "ar" Arabic
   :english English
   :german German
   :french French
   :dutch Dutch
   :italian Italian
   :spanish Spanish
   :turkish Turkish
   :arabic Arabic})

(defmulti ^{:private true} attribute (fn [att token-stream] att))
(defmethod attribute :char-term [_ ^TokenStream ts]
  (.addAttribute ts CharTermAttribute))
(defmethod attribute :offset [_ ^TokenStream ts]
  (.addAttribute ts OffsetAttribute))

(defmulti ^{:private true} attribute-value (fn [att-name att] att-name))
(defmethod attribute-value :char-term [_ ^CharTermAttribute att]
  (.toString att))
(defmethod attribute-value :offset [_ ^OffsetAttribute att]
  {:start (.startOffset att)
   :end (.endOffset att)})

(defn- build-char-map
  "Ex: (build-char-map [\"foo=>bar\" \"a=>z\"])"
  [mappings]
  (let [cm (org.apache.lucene.analysis.charfilter.NormalizeCharMap$Builder.)]
    (doseq [m mappings]
      (let [[match replacement] (clojure.string/split m #"=>")]
        (.add cm match replacement)))
    (.build cm)))

(defmulti ^{:private true} char-filter (fn [x] (or (:type x) x)))
(defmulti ^{:private true} token-filter (fn [x] (or (:type x) x)))
(defmulti ^{:private true} tokenizer (fn [x] (or (:type x) x)))


(defmethod char-filter :mapping [{:keys [mappings]}]
  (fn [rdr]
    (MappingCharFilter. (build-char-map mappings) rdr)))
(defmethod char-filter :html-strip [_]
  (fn [rdr]
    (HTMLStripCharFilter. rdr)))
(defmethod char-filter :pattern-replace [{:keys [pattern replacement]}]
  (fn [rdr]
    (PatternReplaceCharFilter. pattern replacement rdr)))

(defmethod token-filter :standard [_]
  (fn [token-stream]
    (StandardFilter. token-stream)))
(defmethod token-filter :ascii-folding [_]
  (fn [token-stream]
    (ASCIIFoldingFilter. token-stream)))
(defmethod token-filter :length [{:keys [min max] :or {min 0, max Integer/MAX_VALUE}}]
  (fn [token-stream]
    (LengthFilter. token-stream (int min) (int max))))
(defmethod token-filter :stop [{:keys [stopwords ^java.lang.Boolean ignore-case?]
                                :or {ignore-case? true}}]
  (let [stop-set (StopFilter/makeStopSet stopwords ignore-case?)]
    (fn [token-stream]
      (StopFilter.  token-stream stop-set))))

(defmethod tokenizer :standard [_]
  (fn [^Reader rdr]
    (StandardTokenizer. rdr)))
(defmethod tokenizer :edge-n-gram [{:keys [min-gram max-gram] :or {min-gram EdgeNGramTokenizer/DEFAULT_MIN_GRAM_SIZE
                                                                   max-gram EdgeNGramTokenizer/DEFAULT_MAX_GRAM_SIZE}}]
  (fn [^Reader rdr]
    (EdgeNGramTokenizer. rdr (int min-gram) (int max-gram))))

(defmacro ^{:private true} rdr-analyzer [analyzer]
  `(fn [rdr# attributes#]
     (let [ts# (.tokenStream ^Analyzer ~analyzer "field" ^Reader rdr#)
           atts# (for [att# attributes#] (attribute att# ts#))
           ret# (transient [])]
       (.reset ts#)
       (try (while (.incrementToken ts#)
              (conj! ret# (into {} (zipmap attributes#
                                          (map (fn [attribute# att#] (attribute-value attribute# att#))
                                               attributes# atts#)))))
            (.end ts#)
            (persistent! ret#)
            (finally (.close ts#))))))

(defmulti analyzer (fn [x] (or (:type x) x)))
(defmethod analyzer :standard [_] (rdr-analyzer (StandardAnalyzer.)))
(defn text-analyzer [language]
  (analyzer language))
(defmethod analyzer English [_] (rdr-analyzer (EnglishAnalyzer.)))
(defmethod analyzer German [_] (rdr-analyzer (GermanAnalyzer.)))
(defmethod analyzer French [_] (rdr-analyzer (FrenchAnalyzer.)))
(defmethod analyzer Dutch [_] (rdr-analyzer (DutchAnalyzer.)))
(defmethod analyzer Italian [_] (rdr-analyzer (ItalianAnalyzer.)))
(defmethod analyzer Spanish [_] (rdr-analyzer (SpanishAnalyzer.)))
(defmethod analyzer Turkish [_] (rdr-analyzer (TurkishAnalyzer.)))
(defmethod analyzer Arabic [_] (rdr-analyzer (ArabicAnalyzer.)))
(defmethod analyzer :custom [{:as settings}]
  (rdr-analyzer (proxy [Analyzer] []
                  (initReader [^String fieldName, ^Reader reader]
                    (reduce (fn [reader char-filter-spec]
                              ((char-filter char-filter-spec) reader))
                            reader
                            (:char-filter settings)))
                  (createComponents [^java.lang.String fieldName, ^java.io.Reader reader]
                    (let [tokenizer ((tokenizer (:tokenizer settings)) reader)
                          token-stream (reduce (fn [res f]
                                                 ((token-filter f) res))
                                               tokenizer
                                               (:token-filter settings))] 
                      (org.apache.lucene.analysis.Analyzer$TokenStreamComponents. tokenizer token-stream))))))

(defn analyze
  ([analyzer input]
   (analyze analyzer input #{:char-term :offset}))
  ([analyzer input attributes]
   (with-open [rdr (if (string? input)
                     (java.io.StringReader. input)
                     (io/reader input))]
     (analyzer rdr attributes))))

(comment

  (analyze (text-analyzer :english) "An entire generation we don't need")
  (analyze (text-analyzer :standard) "An entire generation we don't need")

  )


