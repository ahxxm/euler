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

(def divisors
  (memoize (fn [n]
    (conj (factors n) 1))))

(defn exp
  [x n]
  (reduce * (repeat n x)))

(defn num->digits
  [n]
  (map #(- (int %) 48) (seq (str n))))
