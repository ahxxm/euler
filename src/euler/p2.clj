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

(defn take-4digit
  ;; figurate numbers f(n)
  [f]
  (->> (range 1M 10000M)
       (map f)
       (filter #(and (>= % 1000)
                     (<= % 9999)))))

(defn is-cyclic?
  ;; the last two digits of each number is the first two digits of the next number (including the last number with the first).
  ([a b]
   (= (subs (str a) 2 4)
      (subs (str b) 0 2)))
  ([arr]
   (loop [-arr arr]
     (if (nil? (second -arr))
       (is-cyclic? (first -arr) (first arr)) ;; end, check end and first
       (if-not (is-cyclic? (first -arr) (second -arr)) ;; else check first and next
         false
         (recur (rest -arr)))))))


(defn any-cyclic?
  [n arr lr]
  (if lr ;; l from left
    (some true? (map #(is-cyclic? n %) arr))
    (some true? (map #(is-cyclic? % n) arr))))

(defn filter-cyclic
  [-left -right]
  [(filter #(any-cyclic? % -right true) -left)
   (filter #(any-cyclic? % -left false) -right)])

(defn solver-61
  [ops]
  (loop [oo (vec (map vec ops))
         i 0]
    (if (and (= [1 1 1 1 1 1] (map count oo))
             (is-cyclic? (map first oo)))
      oo
      (if (= (reduce + (map count oo)) 0)
        false
        ;; else loop from i to next, filter out nums that can't be cyclic
        (let [left  (nth oo i)
              -i    (mod (inc i) 6) ;; hard-coded 6
              right (nth oo -i)
              [l r] (filter-cyclic left right)
              -oo   (assoc oo i l)
              -oo   (assoc -oo -i r)]
          (recur -oo -i))))))

(defn solve-61
  []
  (let [tri (take-4digit #(/ (* % (inc %)) 2)) ;n(n+1)/2
        squ (take-4digit #(* % %)) ;; n*n
        pen (take-4digit #(/ (* % (- (* % 3) 1)) 2)) ;; n(3n−1)/2
        hex (take-4digit #(* % (- (* % 2) 1))) ;; n(2n-1)
        hep (take-4digit #(/ (* % (- (* % 5) 3)) 2)) ;; n(5n−3)/2
        oct (take-4digit #(* % (- (* % 3) 2))) ;; n(3n−2)
        ops [tri squ pen hex hep oct]]
    (->> (first (filter solver-61 (comb/permutations ops)))
         (solver-61)
         (map first)
         (reduce +))))


(defn solve-62
  ;; Find the smallest cube for which exactly five permutations of its digits are cube.
  []
  (loop [i 2
         m {}]
    (let [s (frequencies (str (* i i i)))
          c (m s)]
      (if (= s (frequencies "589323567104"))
        (* i i i)
        (recur (inc i) m))
      #_(if (nil? c)
        (recur (inc i) (assoc m s 1))
        (if (= c 4) ;; found the last one
          (* i i i) ;; 589323567104
          (recur (inc i) (assoc m s (inc c)))))
      )))
