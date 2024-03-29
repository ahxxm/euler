(ns euler.core
  (:require [clojure.string :as str]
            [euler.helpers :refer :all]
            [clojure.math.combinatorics :as comb :refer [count-combinations permutations]])
  (:gen-class))

;; 1
;;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;Find the sum of all the multiples of 3 or 5 below 1000.

#_(->> (range 0 1000)
     (filter #(or (= 0 (mod % 3))
                  (= 0 (mod % 5))))
     (reduce +))


;; 2: Even Fibonacci numbers
;; sum when < 4Million
#_(let [a (atom 1)
      b (atom 2)]
  (defn fib
    [n]
    (let [c (+ @a @b)]
      (reset! a @b)
      (reset! b c)
      c)))

#_(->> (iterate fib 0)
     (take-while #(< % 4000000))
     (filter even?)
     (reduce +)
     (+ 2))


;; 3: Largest prime factor
#_(->> (factors 600851475143)
     (filter is-prime?)
     (apply max))


;; 4:
;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit numbers.

#_(->> (for [k (range 100 1000)
           j (range 100 1000)]
       (* k j))
     (filter is-palindrome)
     (apply max))

;; 5: What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
;; https://rosettacode.org/wiki/Least_common_multiple#Clojure
#_(reduce lcm (range 2 20))

;; 6: Sum square difference

#_(let [numbers (vec (range 1 101))
      s       (reduce + numbers)
      sum-sq  (* s s)
      sq-sum  (reduce + (map #(* % %) numbers))]
  (- sum-sq sq-sum))

;; 7: 10001st prime
;; see 3 for is-prime
#_(->> (iterate inc 2)
     (filter is-prime?)
     (take 10001)
     last)


;; 8: Largest product in a series
(def series "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

#_(let [v (vec series)
      j (- (count series) 13)
      r (atom 0)]
  (loop [i 0]
    (if (= i j)
      @r
      (let [n (map #(- (int %) 48) (subvec v i (+ i 13)))
            p (reduce * n)]
        (when (> p @r)
          (reset! r p))
        (recur (inc i))))))

;; 9: Special Pythagorean triplet
;; find a * b * c where
;; - a*a+b*b=c*c
;; a + b + c = 1000

#_(for [a (range 1 1000)
      b (range 1 1000)
      :let [c (- 1000 a b)]
      :when (and (> c 0)
                 (= (+ (* a a) (* b b))
                    (* c c)))]
  (* a b c))

;; 10:
;; Find the sum of all the primes below two million.
;; TODO: this is slow, how to make it faster?
#_(->> (iterate inc 2)
     (filter is-prime?)
     (take-while #(< % 2000000))
     (reduce +))


;; 11: Largest product in a grid
;; U D L R and diagonally product of 4 numbers, in a 20*20 grid
(def grid
  "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

#_(def nums
  (->> (clojure.string/split-lines grid)
       (map #(clojure.string/split % #" "))
       flatten
       (map #(Integer. %))
       (partition 20)
       vec))

#_(defn safe-nth
  ;; left upper as 0,0
  ;; x: left->right
  ;; y: up->down
  [x y]
  (if (and (< x 20) (< y 20)
           (>= x 0) (>= y 0))
    (nth (nth nums y) x)
    0))

#_(def results
(for [x (range 0 20)
      y (range 0 20)
      :let [;; 4 directions
            i  (safe-nth x y)
            rr (* i
                  (safe-nth (+ x 1) y)
                  (safe-nth (+ x 2) y)
                  (safe-nth (+ x 3) y))
            dd (* i
                  (safe-nth x (+ y 1))
                  (safe-nth x (+ y 2))
                  (safe-nth x (+ y 3)))
            rd (* i
                  (safe-nth (+ x 1) (+ y 1))
                  (safe-nth (+ x 2) (+ y 2))
                  (safe-nth (+ x 3) (+ y 3)))
            ld (* i
                  (safe-nth (- x 1) (+ y 1))
                  (safe-nth (- x 2) (+ y 2))
                  (safe-nth (- x 3) (+ y 3)))
            ]]
  [rr dd rd ld]))

#_(apply max (flatten results))

;; 12: Highly divisible triangular number
;; see day 3 for divisors
#_(let [i (atom 7)
      n (atom 21)]
  (defn tri
    [k]
    (let [->n (+ @i @n)]
      (swap! i inc)
      (reset! n ->n)
      ->n)))

#_(->> (iterate tri 0)
     ;; "over 500" -> >500 -> 501 -> 499+1+self
     (drop-while #(< (count (factors %)) 499))
     first)


;; 13: Large Sum
(def nums
  "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

#_(->> (clojure.string/split-lines nums)
     (map #(BigInteger. %))
     (reduce +)
     str
     (#(subs % 0 10)))


;; 14: Longest Collatz sequence
#_(def collatz-count
  (memoize
   (fn  [n]
     (loop [->n n
            i   1]
       (cond
         (= ->n 1) [i n]
         (even? ->n) (recur (/ ->n 2) (inc i))
         (odd? ->n)  (recur (+ 1 (* ->n 3)) (inc i)))))))

#_(->> (range 1 1000000)
     (map #(collatz-count %))
     (into (sorted-map))
     last)

;; 15:
;; 20×20 grid, only right and down
(count-combinations (range 0 40) 20)

;; 16:
#_(->> (reduce * (repeat 1000 2M))
     str
     seq
     (map #(+ -48 (int %)))
     (reduce +))


;; 17: Number letter counts
;; ... skipped

;; 18: Maximum path sum I

(def tristr
  "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

;; ["75"] ["95" "64"] ...
(def strtri
  (->> (clojure.string/split-lines tristr)
       (map #(clojure.string/split % #" "))
       vec))

#_(defn take-max
  [line1 line2]
  ;; max(i,j) = tri[i][j] + max(tri[i+1][j], tri[i+1][j+1])
  (loop [i  (dec (count line1))
         re '()]
    (if (< i 0)
      (vec re)
      (let [n (nth line1 i)
            l (+ n (nth line2 i))
            r (+ n (nth line2 (inc i)))]
        (if (> l r)
          (recur (dec i) (cons l re))
          (recur (dec i) (cons r re)))))))


#_(loop [t strtri
       i (- (count strtri) 2)]
  (if (< i 0)
    (-> t (nth 0) (nth 0))
    ;; else recur add to lines
    (let [num       (map #(Integer. %) (nth t i))
          next-num  (map #(Integer. %) (nth t (inc i)))]
      (recur (assoc t i (take-max num next-num)) (dec i)))))


;; 19: Count sundays
;; 1 Jan 1901 to 31 Dec 2000
;; how many yyyymm01 are Sunday(s)
#_(let [cal (java.util.Calendar/getInstance)
      end (java.util.Calendar/getInstance)
      sun (java.util.Calendar/SUNDAY)
      startdate #inst "1901-01-01T06:30:33.240-00:00"
      enddate #inst "2000-12-31T06:30:33.240-00:00"]
  (. cal setTime startdate)
  (. end setTime enddate)
  (loop [c 0]
    (if (. cal after end)
      c
      (let [d (. cal get java.util.Calendar/DAY_OF_WEEK)
            m (. cal get java.util.Calendar/DAY_OF_MONTH)]
        (. cal add java.util.Calendar/DATE 1)
        (if (and (= d sun) (= m 1))
          (recur (inc c))
          (recur c))))))

;; 20: Factorial digit sum
;; 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;; 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27
#_(->> (factorial 100)
     str
     seq
     (map #(Character/digit % 10))
     (reduce +))

;; 21: Amicable numbers
#_(let [result (atom [])]
  (loop [n 1]
    (if (= n 10000)
      (reduce + 0 @result)
      (let [m (reduce + 0 (divisors n))
            mn (reduce + 0 (divisors m))]
        (when (and (= mn n)
                   ;; as required, 28 496 6368 can't be included
                   (not (= m n)))
          (swap! result conj n))
        (recur (inc n))))))


;; 22: Names scores
;; sort, i*score, sum
#_(def names (slurp "https://projecteuler.net/project/resources/p022_names.txt"))
#_(defn name->score
  [name]
  (let [lname  (clojure.string/lower-case name)
        scores (map #(+ -96 (int %)) (seq lname))]
    (reduce + scores)))

#_(->> (clojure.string/split names #",")
     (map #(clojure.string/replace % #"\"" ""))
     sort
     (map-indexed (fn [i name] (* (inc i) (name->score name))))
     (reduce +))


;; 23: Non-abundant sums
;; 1 + 2 + 4 + 7 + 14 = 28, <28 deficient, =28 perfect, >28 abundant
;; - find abundant <= 28123
;; - find non

#_(defn is-abundant?
  [n]
  (let [ds (divisors n)]
    (< n (reduce + ds))))

#_(defn not-absum?
  [as n]
  (let [nums   (filter #(< % n) as)
        exists (map #(as (- n %)) nums)]
    (every? nil? exists)))

#_(let [-as (->> (range 1 28123) (filter is-abundant?))
      as  (into #{} -as)
      nums (->> (range 1 28123) (filter (partial not-absum? as)))]
  (reduce + nums))

;; 24: nth permutation
;; https://github.com/clojure/math.combinatorics/blob/4c3d26eec2206e09b8b8c305bfed921966bce6f6/src/main/clojure/clojure/math/combinatorics.cljc#L419

;; 25: 1000-digit Fibonacci number
;; see 2
#_(let [a (atom 1M)
      b (atom 2M)
      i (atom 3)]
  (defn fib-indexed
    [n]
    (let [c (+ @a @b)]
      (reset! a @b)
      (reset! b c)
      (swap! i inc)
      [@i c])))


#_(->> (iterate fib-indexed 0)
     ;; TODO: why 0 appears first in iterate result?
     rest
     (drop-while #(< (->> % second str count) 1000))
     first
     first)


;; 26: Reciprocal cycles
;; 1/7 => 0.(142857) => 6
#_(defn find-reciprocal
  ;; https://www.mathblog.dk/project-euler-26-find-the-value-of-d-1000-for-which-1d-contains-the-longest-recurring-cycle/
  [num div]
  (loop [seen (vec (take (inc div) (repeat 0)))
         v    1
         pos  0]
    (if (or (zero? v)
            (not (= 0 (nth seen v))))
      [(- pos (nth seen v)) div]
      (recur (assoc seen v pos)
             (mod (* v 10) div)
             (inc pos)))))

#_(->> (into (sorted-map) (map  #(find-reciprocal 1 %) (range 1 1000)))
     last
     second)

;; 27: Quadratic primes
;; n^2+an+b , where |a|<1000 and |b|≤1000
;; Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.
#_(defn count-primes
  [a b]
  (let [primes (take-while is-prime? (map
                                      #(+ b (* a %) (* % %))
                                      (iterate inc 0)))]
    (count primes)))

#_(let [max-a (atom 0)
      max-b (atom 0)
      cur   (atom 0)]
  (loop [a -1000
         b -1000]
    (let [primes (count-primes a b)]
      (when (> primes @cur)
        (reset! cur primes)
        (reset! max-a a)
        (reset! max-b b)))
    (if (= a 1001)
      (* @max-a @max-b)
      (if (= b 1001)
        (recur (inc a) -1000)
        (recur a (inc b))))))


;; 28: Number spiral diagonals
;; https://oeis.org/A200975
#_(let [n 1001]
  (loop [i   1
         sum 0]
    (if (= i (/ (inc n) 2))
      (+ sum (* n n))
      (let [n1 (+ (* 4 i i) (* -4 i) 1)
            n2 (+ (* 4 i i) (* -4 i) 1 (* 1 2 i))
            n3 (+ (* 4 i i) (* -4 i) 1 (* 2 2 i))
            n4 (+ (* 4 i i) (* -4 i) 1 (* 3 2 i))]
        (recur (inc i) (+ sum n1 n2 n3 n4))))))


;; 29: Distinct powers
;; How many distinct terms are in the sequence generated by a^b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
#_(let [seen (atom #{})]
  (loop [a 2M
         b 2M]
    (if (= a 101M)
      (count @seen)
      (if (= b 101M)
        (recur (inc a) 2M)
        (let [p (exp a b)]
          (swap! seen conj p)
          (recur a (inc b)))))))


;; 30: Digit fifth powers
;; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
;; since (exp 9 5) => 59049, take 360000 as upper
#_(loop [sum 0
       n   2]
  (if (= n 360000)
    sum
    (let [pow-sum (reduce + (map #(exp % 5) (num->digits n)))]
      (if (= pow-sum n)
        (recur (+ sum n) (inc n))
        (recur sum (inc n))))))

;; 31: Coin Sums
;; How many different ways can £2 be made using any number of coins?
;;
;; https://www.xarg.org/puzzle/project-euler/problem-31/
#_(let [coins [1 2 5 10 20 50 100 200]
      n     200 ;; target
      r     (->> (take (inc n) (iterate inc 0))
                 (map #(vec (list % 0)))
                 (into (sorted-map))
                 atom)]
  ;; init
  (swap! r assoc 0 1)
  (dorun ;; important..
  (for [coin coins
        j    (range coin (inc n))]
    (let [prev (@r j)
          new  (@r (- j coin))]
      (swap! r assoc j (+ prev new)))))
  (last (last @r)))

;; 32: Pandigital products
;; n-digit number is pandigital if it makes use of all the digits 1 to n exactly once
;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;; http://www.worldofnumbers.com/ninedig1.htm
;; 4*1963 is one answer..
#_(let [s (atom #{})]
  (loop [a 1
         b 1]
    (if (= a 2000)
      s
      (if (= b 2000)
        (recur (inc a) 1)
        (let [prod (* a b)]
          (when (prod-concat-pandigital? a b)
            (swap! s conj prod))
          (recur a (inc b))))))
  (reduce + @s))


;; 33: Digit cancelling fractions
;; (/ 49 98)

#_(defn digit-cancel?
  [a b]
  (let [f  (/ a b)
        ad (->> a str seq (map #(- (int %) 48)) vec)
        bd (->> b str seq (map #(- (int %) 48)) vec)
        a1 (first ad)
        a2 (second ad)
        b1 (first bd)
        b2 (second bd)]
    (or
     (and (= a1 b1) (> b2 0)
          (= (/ a2 b2) f))
     (and (= a2 b1) (> b2 0)
          (= (/ a1 b2) f))
     (and (= a2 b2)
          (= (/ a1 b1) f)))))


#_(let [f (atom #{})]
  (loop [a 10
         b 10]
    (when (and (digit-cancel? a b)
               (> (mod a 10) 0)
               (> (mod b 10) 0)
               (< (/ a b) 1))
      (swap! f conj (/ a b)))
    (if (= a 100)
      (reduce * (filter #(< % 1) @f))
      (if (= b 100)
        (recur (inc a) 10)
        (recur a (inc b))))))

;; 34: Digit factorials
;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

#_(defn ->digits
  [n]
  (->> n str seq (map #(- (int %) 48)) vec))

#_(defn fact-sum
  [n]
  [n (reduce + (map factorial (->digits n)))])

;; (fact-sum 9999999) => [9999999 2540160M]
#_(->> (range 10M 3000000M)
     (map fact-sum)
     (filter #(= (first %) (second %)))
     (map first)
     (reduce +))
;;(+ 145 40585)

;; 35: Circular primes
;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
;; How many circular primes are there below one million?
;; https://stackoverflow.com/questions/28721600/idiomatic-string-rotation-in-clojure
#_(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

#_(defn ->rotation
  [n]
  (let [rotations (rotations (str n))]
    (map #(BigDecimal. (apply str %)) rotations)))

#_(->> (range 2 1000000)
     (filter #(every? is-prime? (->rotation %)))
     count)

;; 36: Double-base palindromes
(defn both-palindrome
  [n]
  (and (is-palindrome n)
       (is-palindrome (Integer/toBinaryString n))))

(->> (range 1 1000000)
     (filter both-palindrome)
     (reduce +))

;; 37: Truncatable primes
;;
#_(defn truncate-left
  [n]
  (loop [digits (seq (str n))
         nums   []]
    (if (empty? digits)
      nums
      (recur (rest digits) (conj nums (BigDecimal. (apply str digits)))))))

#_(defn truncate-right
  [n]
  (loop [digits (seq (str n))
         nums   []]
    (if (empty? digits)
      nums
      (recur (drop-last digits) (conj nums (BigDecimal. (apply str digits)))))))

#_(->> (iterate inc 9)
     (filter #(every? is-prime? (concat (truncate-left %)
                                        (truncate-right %))))
     (take 11)
     (reduce +))

;; 38: Pandigital multiples
;; greedy cut: 4-digit * 2, 4+5 to pandigital
#_(defn double-pan?
  [n]
  (let [k      (* n 2)
        digits (into #{} (str n k))]
    (= digits #{\1 \2 \3 \4 \5 \6 \7 \8 \9})))

#_(->> (range 9999 5000 -1)
     (filter double-pan?)
     first)

;; 39: Integer right triangles
;; a + b + c <= 1000
;; a*a+b*b=c*c
;; a,b,c => int
#_(defn count-triangles
  [p]
(let [;p     120
      half  (int (/ p 2))
      pairs (atom #{})]
  (loop [a 1
         b 1]
    (if (= a half)
      [p (count @pairs)]
      (if (= b half)
        (recur (inc a) 1)
        (let [c (- p a b)]
          (when (= (* c c)
                   (+ (* a a) (* b b)))
            (swap! pairs conj #{a b}))
          (recur a (inc b))))))))

#_(->> (range 20 1000)
     (map count-triangles)
     (into (sorted-map))
     (apply max-key val)
     first)


;; 40: Champernowne's constant
;; An irrational decimal fraction is created by concatenating the positive integers:
;; 0.123456789101112131415161718192021...
;; d(x) is x th digit
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

#_(let [digits (->> (iterate inc 0)  ;; since nth is 0-based index
                  (map str)
                  (apply concat)
                  (take 1000001))]
  (reduce * (map #(- (int (nth digits %)) 48)
                 [1 10 100 1000 10000 100000 1000000])))

;; 41: Pandigital prime
;; all 8&9 digits numbers can be divided by 3 <= 1+2+3..+8=36
#_(->> (iterate dec 9999999)
     (filter is-prime?)
     (filter is-pandigital?)
     (take 1))


;; 42: Coded triangle numbers
#_(def -words (slurp "https://projecteuler.net/project/resources/p042_words.txt"))
#_(def words (map #(str/replace % "\"" "") (str/split -words #",")))

#_(defn word->score
  [w]
  (let [l (str/lower-case w)]
    (reduce + (map #(- (int %) 96) (seq l)))))

#_(def triangles
  (->> (iterate inc 0)
       (take 1000)
       (map #(/ (* % (inc %)) 2))
       (into #{})))

#_(->> (map word->score words)
     (filter #(triangles %))
     count)

;; 43: Sub-string divisibility
#_(->> (permutations [0 1 2 3 4 5 6 7 8 9])
     ;; or permutate from 1 0 2 3 4 5 6 6 7 8 9
     (filter #(not= 0 (nth % 0)))
     (filter (fn [coll]
               (and (= 0 (mod (nth coll 3) 2))
                    (= 0 (mod (+ (nth coll 2) (nth coll 3) (nth coll 4)) 3))
                    (or (= 0 (nth coll 5)) (= 5 (nth coll 5)))
                    (= 0 (mod (+ (* 100 (nth coll 4)) (* 10 (nth coll 5)) (nth coll 6)) 7))
                    (= 0 (mod (+ (* 100 (nth coll 5)) (* 10 (nth coll 6)) (nth coll 7)) 11))
                    (= 0 (mod (+ (* 100 (nth coll 6)) (* 10 (nth coll 7)) (nth coll 8)) 13))
                    (= 0 (mod (+ (* 100 (nth coll 7)) (* 10 (nth coll 8)) (nth coll 9)) 17))
                    )))
     (map #(BigDecimal. (apply str %)))
     (reduce +))


;; 44: Pentagon numbers
;; n * (3n-1) / 2
;; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?
#_(let [-nums (->> (iterate inc 1)
                 (map #(/ (* % (- (* % 3) 1)) 2))
                 (take 2500))
      nums  (vec -nums)
      numss (into #{} nums)]
  (->> (comb/selections nums 2)
       (filter (fn [[a b]] (and (numss (Math/abs (- a b)))
                                (numss (+ a b)))))))

;; 45: Triangular, pentagonal, and hexagonal
#_(let [-tri (->> (iterate inc 1)
                (map #(/ (* % (inc %)) 2))
                (take 100000))
      -pen (->> (iterate inc 1)
                (map #(/ (* % (- (* % 3) 1)) 2))
                (take 100000))
      -hex (->> (iterate inc 1)
                (map #(* % (- (* % 2) 1)))
                (take 100000))
      tri  (into #{} -tri)
      pen  (into #{} -pen)
      hex  (into #{} -hex)]
  (clojure.set/intersection tri pen hex))


;; 46: Goldbach's other conjecture
;; What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
(defn is-twice-square?
  [n]
  (let [x (Math/sqrt (/ n 2))]
    (= n (* (int x) (int x)))))

#_(let [primes (take 10000 (filter is-prime? (iterate inc 1)))
      squares (take 100 (map #(* 2 % %) (iterate inc 0)))
      sums (into #{} (for [p primes
                           s squares]
                       (+ p s)))]
  (loop [n 3]
    (if (sums n)
      (recur (+ n 2))
      n)))


;; 47: Find the first four consecutive integers to have four distinct prime factors each.
;; What is the first of these numbers?
(def count-factors
  (memoize #(->> (factors %) (filter is-prime?) count)))

#_ (loop [i (* 2 3 5 7)]
  (let [fs (map count-factors [(+ i 0) (+ i 1)
                               (+ i 2) (+ i 3)])]
    (if (every? #(>= % 4) fs)
      i
      (recur (inc i)))))

;; 48: Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
#_(->> (range 1M 1001M)
     (map #(pow % %))
     (reduce +)
     str
     (take-last 10)
     (apply str))

;; 49: Prime permutations
;; 1487, 4817, 8147
;; (i) each of the three terms are prime, and,
;; (ii) each of the 4-digit numbers are permutations of one another.

(defn perm->num
  ;; 4 Character
  [perm]
  (->> perm
       (apply str)
       (BigDecimal.)))

(defn incr-sequence?
  [[a b c]]
  (= (- a b) (- b c)))

(defn perm-incr?
  [[counter _]]
  (let [;;counter {1 1, 2 1, 3 1, 7 1}
        digits (mapcat (fn [[x n]] (repeat n x)) counter)
        perms (comb/permutations digits)
        perm-primes (filter is-prime? (map perm->num perms))
        choices (comb/combinations perm-primes 3)]
    ;; any choose 3 from list can form an Arithmetic sequence
    (not-empty (filter incr-sequence? choices))))

#_(->> (range 1000 10000)
     (filter is-prime?)
     (map str)
     (map frequencies)
     (frequencies)
     (filter #(>= (val %) 3))
     (sort-by val >)
     (filter perm-incr?)) ;; => [{\2 1, \6 1, \9 2} 4]

#_(->> {\2 1, \6 1, \9 2}
     (mapcat (fn [[x n]] (repeat n x)))
     (comb/permutations)
     (map perm->num)
     (filter is-prime?))

;; 50: Consecutive prime sum
;; Which prime, below one-million, can be written as the sum of the most consecutive primes?

#_ (apply max
(for [n [2 3 5 7 11 13 17]]
  (loop [sum [n]
         p   (filter is-prime? (iterate inc (inc n)))]
    (let [prime (first p)
          -last (last sum)
          -sum (+ -last prime)]
      (if (> -sum 1000000)
        (last (filter is-prime? sum))
        (recur (conj sum -sum) (rest p)))))))


;; 51: https://www.mathblog.dk/project-euler-51-eight-prime-family/
(defn primes-under2 [n]
  (let [sieve (transient (set (cons 2 (range 3 n 2))))]
    (loop [s sieve
           f 3]
      (cond (> (* f f) n) (persistent! s)
            :else (recur (reduce disj! s (range (* f f) n f)) (inc f))))))



#_(time (count (primes-under2 10000000)))


;; 52: Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
(defn mul-6
  [n]
  (let [nums (map #(* % n) (range 2 7))]
    (conj nums n)))

(defn ->digits-set
  [n]
  (->> n
     str
     vec
     (into #{})))

(defn solve-52
  ;; => 142857
  []
  (loop [i 1]
    (let [nums (map ->digits-set (mul-6 i))
          numset (into #{} nums)]
      (if (= 1 (count numset))
        i
        (recur (inc i))))))


;; 53: How many, not necessarily distinct, values of (choose n r), where r<=n, 1<n<=100
;; are >=1000000
(defn all-valid-combo
  [n]
  (let [items (range 0 n)
        combos (for [i (range 1 n)]
                 (comb/count-combinations items i))]
    (filter #(> % 1000000) combos)))

(defn solve-53
  []
  (let [results (map all-valid-combo (range 1 101))]
    (reduce + (map count results))))


;; 54: skip

;; 55: A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
;; In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations,
;; How many Lychrel numbers are there below ten-thousand?

(defn sum-reverse
  [n]
  (let [reversed (BigDecimal. (->> n
                                (str)
                                (reverse)
                                (clojure.string/join)))]
    (+ reversed n)))

(defn is-lychrel?
  [n]
  (loop [i 0
         k n]
    (if (> i 50)
      true
      (let [sum (sum-reverse k)]
        (if (is-palindrome sum)
          false
          (recur (inc i) sum))))))

(defn solve-55
  []
  (let [results (map is-lychrel? (range 0 10001))]
    (count (filter true? results))))


;; 56: Powerful digit sum
;; a**b where a,b<100
(defn digit-sum
  [n]
  (->> n
       (str)
       (vec)
       (map #(- (int %) 48))
       (reduce +)))

(defn solve-56
  []
  (let [results (for [a (range 0 101)
                      b (range 0 101)]
                  (digit-sum (pow (BigDecimal. a)
                                  (BigDecimal. b))))]
    (apply max results)))

;; 57: Square root convergents

(defn count-digits
  [n]
  (->> n
       str
       vec
       count))

(defn solve-57
  [n]
  (loop [a 2M
         b 3M
         i 0  ;; iteration
         m 0] ;; count
    (if (> i n)
      m
    (let [c (+ a b)  ;; the new a
          d (+ a c)] ;; the new b, frac being b/a
      (if (> (count-digits d)
             (count-digits c))
        (recur c d (inc i) (inc m))
        (recur c d (inc i) m))))))

;; 58: Spiral Primes
;; Starting with 1 and spiralling anticlockwise
;; 5 4 3
;; 6 1 2
;; 7 8 9
;; what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

(defn assoc-vov
  [v x y val]
  (let [-line (nth v y)
        line  (assoc -line x val)]
    (assoc v y line)))

(defn should-turn
  [x y n dir v]
  (let [[-x -y] dir
        x (+ x -x)
        y (+ y -y)]
    (if (or (< x 0) (>= x n)
            (< y 0) (>= y n))
      true ;; out of bound, or value already set
      (not= (nth (nth v y) x) 0))))


(defn gen-spiral ;; use 1-d array?
  [n]
  ;; n should be 2*k+1
  ;; init with southeast value, spiral inside until 1
  ;; return vector of vector
  (let [sqrt   (* n n)
        line   (vec (repeat n 0))
        lines  (vec (repeat n line))
        dirs   [[-1 0] [0 -1] [1 0] [0 1]]] ;; filler direction always start with <-
    ;; fill the square spiral
    (loop [i   sqrt
           dir 0
           x   (dec n)
           y   (dec n)
           v   lines]
      (if (= i 1)
        (assoc-vov v x y i)
        (let [-dir     (nth dirs (mod dir 4))
              turn?   (should-turn x y n -dir v)
              [-x -y] -dir]
          ;; if turn then don't assoc, just change direction
          ;; else assoc and process
          (if turn?
            (recur i (inc dir) x y v)
            (recur (dec i) dir (+ x -x) (+ y -y) (assoc-vov v x y i))))))))


(defn diag-nums
  [v]
  (let [n (count (nth v 0))]
    (loop [i    0
           vals []]
      (if (= i n)
        vals
        (let [line (nth v i)]
          ;; 1-2 nums, 1 for midpoint
          (if (= n (inc (* i 2)))
            (recur (inc i) (conj vals (nth line i)))
            (recur (inc i) (conj vals (nth line i) (nth line (- n i 1))))))))))

(defn cant-solve-58
  ;; the answer is 26xxx though
  []
  (loop [i 10000]
    (let [n (inc (* i 2))
          spiral (gen-spiral n)
          nums (diag-nums spiral)
          primes (filter is-prime? nums)
          ratio (/ (count primes) (count nums))]
      (if (< ratio 0.1)
        n
        (recur (+ i 10000))))))

(defn solve-58
  ;; only generate diagnal numbers
  []
  (loop [sl 4  ;; next len
         c  9  ;; cornor
         p  3] ;; primes
    (let [x (+ c sl)
          y (+ x sl)
          z (+ y sl) ;; next 3 numbers
          p (+ p (->> [x y z] (filter is-prime?) count))
          ratio (/ p (inc (* 2 sl)))]
      (if (< ratio 0.1)
        (inc sl)
        (recur (+ sl 2) (+ z sl) p)))))


(defn pad
  [key n]
  (clojure.string/join
   (take n (repeat key))))

(defn xor
  ;; key string, msg as int array
  [-key msg]
  (let [kk (pad -key (count msg))
        r   (mapv bit-xor (->> kk vec (map int)) msg )]
    (apply str (map char r))))

;; key be "exp"
;; An extract taken from the introduction of one of Euler's most celebrated papers, "De summis serierum reciprocarum" [On the sums of series of reciprocals]: I have recently found, quite unexpectedly, an elegant expression for the entire sum of this series 1 + 1/4 + 1/9 + 1/16 + etc., which depends on the quadrature of the circle, so that if the true sum of this series is obtained, from it at once the quadrature of the circle follows. Namely, I have found that the sum of this series is a sixth part of the square of the perimeter of the circle whose diameteris 1; or by putting the sum of this series equal to s, it has the ratio sqrt(6) multiplied by s to 1 of the perimeter to the diameter. I will soon show that the sum of this series to be approximately 1.644934066842264364; and from multiplying this number by six, and then taking the square root, the number 3.141592653589793238 is indeed produced, which expresses the perimeter of a circle whose diameter is 1. Following again the same steps by which I had arrived at this sum, I have discovered that the sum of the series 1 + 1/16 + 1/81 + 1/256 + 1/625 + etc.
;; also depends on the quadrature of the circle. Namely, the sum of this multiplied by 90 gives the biquadrate (fourth power) of the circumference of the perimeter of a circle whose diameter is 1. And by similar reasoning I have likewise been able to determine the sums of the subsequent series in which the exponents are even numbers.
(defn solve-59
  []
  (let [cipher (slurp "https://projecteuler.net/project/resources/p059_cipher.txt")
        splits (clojure.string/split cipher #",")
        arr    (map #(Integer. %) splits)
        keys   (for [a (map char (range 97 123))
                     b (map char (range 97 123))
                     c (map char (range 97 123))]
                 [a b c])]
    (loop [k  (first keys)
           ks (rest keys)]
      (if-not (nil? k)
        (let [kk (apply str k)
              text (xor kk arr)
              c (count (re-seq #"the" text))]
          (if (< c 10) ;; 10 after some test
            (recur (first ks) (rest ks))
            (reduce + (map int text))))))))


(defn main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
