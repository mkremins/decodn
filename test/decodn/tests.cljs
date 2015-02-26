(ns decodn.tests
  (:require [cljs.test :refer-macros [deftest is testing]]
            [decodn.core :as decodn]))

(defn read-form [s]
  (first (:children (decodn/read-document s))))

(deftest read-atoms
  (is (= (read-form "foo")  {:type :symbol :text "foo"}))
  (is (= (read-form ":bar") {:type :keyword :text ":bar"}))
  (is (= (read-form "42")   {:type :number :text "42"}))
  (is (= (read-form "nil")  {:type :nil :text "nil"}))
  (is (= (read-form "true") {:type :bool :text "true"}))
  (is (= (read-form "\\")   {:type :char :text "\\"})))

(deftest read-stringlikes
  (is (= (read-form "\"foo\"")  {:type :string :text "foo"}))
  (is (= (read-form "#\"bar\"") {:type :regex :text "bar"})))

(deftest read-collections
  (is (= (read-form "(foo bar)")
         {:type :seq
          :children [{:type :symbol :text "foo"}
                     {:type :symbol :text "bar"}]}))
  (is (= (read-form "#(+ % -10)")
         {:type :fn
          :children [{:type :symbol :text "+"}
                     {:type :symbol :text "%"}
                     {:type :number :text "-10"}]}))
  (is (= (read-form "[a [b] c]")
         {:type :vec
          :children [{:type :symbol :text "a"}
                     {:type :vec :children [{:type :symbol :text "b"}]}
                     {:type :symbol :text "c"}]}))
  (is (= (read-form "#{{0 Infinity}}")
         {:type :set
          :children [{:type :map
                      :children [{:type :number :text "0"}
                                 {:type :number :text "Infinity"}]}]})))

(deftest read-wrapped
  (is (= (read-form "'foo")
         {:type :seq
          :children [{:type :symbol :text "quote"}
                     {:type :symbol :text "foo"}]}))
  (is (= (read-form "@bar")
         {:type :seq
          :children [{:type :symbol :text "deref"}
                     {:type :symbol :text "bar"}]}))
  (is (= (read-form "#'baz")
         {:type :seq
          :children [{:type :symbol :text "var"}
                     {:type :symbol :text "baz"}]}))
  (is (= (read-form "`foo")
         {:type :seq
          :children [{:type :symbol :text "syntax-quote"}
                     {:type :symbol :text "foo"}]}))
  (is (= (read-form "~bar")
         {:type :seq
          :children [{:type :symbol :text "unquote"}
                     {:type :symbol :text "bar"}]}))
  (is (= (read-form "~@baz")
         {:type :seq
          :children [{:type :symbol :text "unquote-splicing"}
                     {:type :symbol :text "baz"}]})))

(deftest read-prefixed
  (is (= (read-form "^{:private true} foo")
         {:type :symbol :text "foo"
          :meta {:type :map :children [{:type :keyword :text ":private"}
                                       {:type :bool :text "true"}]}}))
  (is (= (read-form "#foo/bar :baz")
         {:type :keyword :text ":baz" :tag "foo/bar"})))

(deftest skip-comments
  (is (= (read-form "#_foo bar") {:type :symbol :text "bar"}))
  (is (= (read-form "; testing \n baz") {:type :symbol :text "baz"}))
  (is (= (read-form "#!/bin/cljs\n42") {:type :number :text "42"})))
