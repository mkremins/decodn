(ns decodn.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]))

;; helper fns

(defn error! [msg]
  (throw (ex-info msg {})))

(defn normalize-newlines [string]
  (str/join \newline (str/split-lines string)))

(def whitespace? #{\space \newline \return \tab \,})

;; collections

(declare read-forms)

(defn read-between [opener closer chars]
  (loop [[ch & more] (rest chars)
         buffer []
         nesting-level 0]
    (condp = ch
      opener
        (recur more (conj buffer ch) (inc nesting-level))
      closer
        (if (zero? nesting-level)
          [more (read-forms buffer)]
          (recur more (conj buffer ch) (dec nesting-level)))
      nil
        (error! (str "Unmatched delimiter: " opener))
      ;else
        (recur more (conj buffer ch) nesting-level))))

(defn read-coll [type opener closer chars]
  (let [[chars items] (read-between opener closer chars)]
    [chars {:type type :children items}]))

(def read-fn  (partial read-coll :fn  \( \)))
(def read-map (partial read-coll :map \{ \}))
(def read-seq (partial read-coll :seq \( \)))
(def read-set (partial read-coll :set \{ \}))
(def read-vec (partial read-coll :vec \[ \]))

;; strings & regexes

(defn read-string [chars]
  (loop [[ch & more] (rest chars)
         buffer []
         escape-next? false]
    (if escape-next?
      (recur more (conj buffer ch) false)
      (case ch
        \" [more {:type :string :text (str/join buffer)}]
        \\ (recur more buffer true)
        nil (error! "Unmatched delimiter: \"")
        (recur more (conj buffer ch) false)))))

(defn read-regex [chars]
  (let [[chars string-form] (read-string chars)]
    [chars (assoc string-form :type :regex)]))

;; simple atom forms

(def numeric?
  (let [int? #(re-matches #"^(\-|\+)?([0-9]+|Infinity)$" %)
        float? #(re-matches #"^(\-|\+)?([0-9]+(\.[0-9]+)?)$" %)
        ratio? #(every? int? (str/split % #"/" 2))]
    (some-fn int? float? ratio?)))

(defn parse-atom
  "Given a `token` string representing a single (non-string, non-regex) atom
  form, returns a parse tree node representing the parsed form."
  [token]
  {:text token :type (cond (#{"true" "false"} token) :bool
                           (= (first token) \\) :char
                           (= (first token) \:) :keyword
                           (= token "nil") :nil
                           (numeric? token) :number
                           :else :symbol)})

(defn read-atom [chars]
  (let [[taken chars] (split-with (complement whitespace?) chars)]
    [chars (parse-atom (str/join taken))]))

;; quotation-related forms

(declare read-form)

(defn wrap [head form]
  {:type :seq :children [{:type :symbol :text (name head)} form]})

(defn read-wrapped [head chars]
  (let [[chars form] (read-form (rest chars))]
    [chars (wrap head form)]))

(def read-deref (partial read-wrapped 'deref))
(def read-quote (partial read-wrapped 'quote))
(def read-syntax-quote (partial read-wrapped 'syntax-quote))

(defn read-unquote [chars]
  (let [chars (rest chars)
        splicing? (= (first chars) \@)
        [chars form] (read-form (cond-> chars splicing? rest))]
    [chars (wrap (if splicing? 'unquote-splicing 'unquote) form)]))

(defn read-var [chars]
  (let [[chars form] (read-form (rest chars))]
    (assert (= (:type form) :symbol)
      (str "Var literal name part must be a symbol, got: " form))
    [chars (wrap 'var form)]))

;; metadata & tagged literals

(defn read-meta [chars]
  (let [[chars meta-form] (read-form (rest chars))
        [chars form] (read-form chars)]
    [chars (assoc form :meta meta-form)]))

(defn read-tagged-literal [chars]
  (let [[chars tag] (read-form chars)
        _ (assert (= (:type tag) :symbol)
            (str "Tagged literal tag must be a symbol, got: " tag))
        [chars form] (read-form chars)]
    [chars (assoc form :tag (:text tag))]))

;; generic interface

(defn skip-rest-of-line [chars]
  (rest (drop-while (partial not= \newline) chars)))

(defn read-dispatch [chars]
  (when-let [ch (first chars)]
    (case ch
      \( (read-fn chars)
      \{ (read-set chars)
      \" (read-regex chars)
      \' (read-var chars)
      \! (read-form (skip-rest-of-line chars))
      \_ (read-form (first (read-form (rest chars))))
      (read-tagged-literal chars))))

(defn read-form
  "Attempts to read a single form from the `chars`. If successful, returns a
  tuple `[chars' node]` of the remaining characters and a parse tree node
  representing the form that was read. Otherwise, returns nil."
  [chars]
  (when-let [ch (first chars)]
    (if (whitespace? ch)
      (recur (drop-while whitespace? chars))
      (case ch
        \( (read-seq chars)
        \[ (read-vec chars)
        \{ (read-map chars)
        \) (error! "Unmatched delimiter: )")
        \] (error! "Unmatched delimiter: ]")
        \} (error! "Unmatched delimiter: }")
        \" (read-string chars)
        \; (recur (skip-rest-of-line chars))
        \@ (read-deref chars)
        \' (read-quote chars)
        \` (read-syntax-quote chars)
        \~ (read-unquote chars)
        \^ (read-meta chars)
        \# (read-dispatch (rest chars))
        (read-atom chars)))))

(defn read-forms
  "Reads a sequence of forms from the `chars` and returns a vector containing
  all of the representative parse tree nodes in the order that they were read."
  [chars]
  (loop [chars chars forms []]
    (if-let [[chars form] (read-form chars)]
      (recur chars (conj forms form))
      forms)))

(defn read-document
  "Reads all top-level forms from the string of Clojure `source` code and
  returns a parse tree representing the entire document."
  [source]
  {:type :program :children (read-forms (normalize-newlines source))})
