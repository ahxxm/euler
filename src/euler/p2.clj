(ns euler.p2
  (:require [clojure.string :as str]
            [euler.helpers :refer :all]
            [clojure.math.combinatorics :as comb :refer [count-combinations permutations]])
  (:gen-class))





;; problem 60-119
(defn concat-prime?
  [[a b]]
  (and (->> [a b]
            (apply str)
            (bigdec)
            (is-prime?))
       (->> [b a]
            (apply str)
            (bigdec)
            (is-prime?))))

(defn select-distinct
  [items n]
  (let [selections (comb/selections items n)]
    (filter #(= (count %)
                (count (distinct %))) selections)))

(defn all-concat-prime?
  [v]
  (let [choices    (select-distinct v 2)
        non-primes (drop-while true? (map concat-prime? choices))]
    (nil? (first non-primes))))

(defn solve-60
  [] ;; FIXME: slow
  (let [primes (->> (iterate inc 3)
                    (filter is-prime?)
                    (take-while #(< % 8500))
                    (map #(BigDecimal. %))
                    vec)
        sels    (select-distinct primes 5)
        choices (filter all-concat-prime? sels)]
    (reduce + (first choices))))
