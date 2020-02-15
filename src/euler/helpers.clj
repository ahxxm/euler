(ns euler.helpers)

(defn factors
  [n]
  (let [s (->> (range 2 (inc (-> n Math/sqrt int)))
               (filter #(= 0 (mod n %)))
               (reduce conj #{}))]
    (reduce conj s (map #(/ n %) s))))

(defn is-prime
  [n]
  (let [k (-> n Math/sqrt int)]
    (every? (complement zero?) (map #(mod n %) (range 2 (inc k))))))

(def is-prime?
  (memoize #(and (> % 1)
                 (is-prime %))))

(defn is-palindrome
  [n]
  (let [s (str n)]
    (= (seq s) (reverse s))))

(defn gcd
  ;; a>b
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn factorial
  [n]
  (reduce * 1M (range 1 (inc n))))

(defn pow
  [n x]
  (reduce * (repeat x n)))

(def divisors
  (memoize (fn [n]
    (conj (factors n) 1))))

(defn exp
  [x n]
  (reduce * (repeat n x)))

(defn num->digits
  [n]
  (map #(- (int %) 48) (seq (str n))))

(defn is-pandigital?
  [a]
  (let [-chars (into #{} (str a))
        d      (count -chars)
        dd     (count (str a))
        digits (into #{} (take dd [\1 \2 \3 \4 \5 \6 \7 \8 \9]))]
    (and (= d dd) ;; exactly once
         ;; 1-n
         (= digits -chars))))

(defn prod-concat-pandigital?
  [a b]
  (let [prod (* a b)
        strs (str a b prod)]
    ;; 9 digits
    (and (= (count strs) 9)
         ;; 1-9
         (= #{\1 \2 \3 \4 \5 \6 \7 \8 \9} (into #{} strs)) )))
