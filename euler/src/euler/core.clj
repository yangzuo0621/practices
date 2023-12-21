(ns euler.core)

;; https://projecteuler.net/problem=1
;; Multiples of 3 and 5
(defn sum-of-multiples
  "sum of all the multiples of 3 or 5 below 1000: 233168"
  []
  (reduce + (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))) (range 1 1000))))

;; https://projecteuler.net/problem=3
;; Largest Prime Factor
(defn all-primes [n]
  (let [scope (range 2 (int (Math/sqrt n)))
        coll (range 2 n)]
    (loop [s scope result coll]
      (if (empty? s)
        result
        (recur (rest s) 
               (filter #(or (not= 0 (mod % (first s))) (= % (first s))) 
                       result))))))

(defn largest-prime-factor
  "largest prime factor of 600851475143: 6857"
  []
  (let [target 600851475143]
    (first (drop-while #(not= 0 (mod target %))
                       (reverse (all-primes (int (Math/sqrt target))))))
    )
)

;; https://projecteuler.net/problem=4
;; Largest Palindrome Product
(defn is-palindrome?
  [n]
  (= (seq (str n)) (reverse (seq (str n)))))

(defn largest-palindrome-product
  "largest palindrome made from the product of two 3-digit numbers: 906609"
  []
  (let [products (for [x (range 100 1000)
                       y (range 100 1000)]
                   (* x y))]
    (first (drop-while #(not (is-palindrome? %)) (reverse (sort products)))))
)

;; https://projecteuler.net/problem=5
;; Smallest Multiple
(defn smallest-multiple
  "smallest positive number that is evenly divisible by all of the numbers from 1 to 20: 232792560"
  []
  (reduce 
    (fn [acc x]
      (if (= 0 (mod acc x))
        acc
        (* acc (denominator (/ acc x))))
    )
    2520 ;; smallest number that can be divided by each of the numbers from 1 to 10 without any remainder
    (range 11 (inc 20))
  )
)

;; https://projecteuler.net/problem=6
;; Sum Square Difference
(defn sum-of-square-difference
  "difference between the sum of the squares of the first one hundred natural numbers and the square of the sum: 25164150"
  []
  (let [power-of-two (fn [n] (reduce * (repeat 2 n)))]
    (- (power-of-two (reduce + (range 1 101)))
       (reduce + (map power-of-two (range 1 101))))
  )
)

;; https://projecteuler.net/problem=7
;; 10001st Prime
(defn prime? [n]
  (every? #(not= 0 (mod n %)) (range 2 (inc (int (Math/sqrt n))))))

(defn _10001st-prime
  "10001st prime: 104743"
  []
  (first (drop 10000 (filter prime? (iterate inc 2))))
  ;;(nth (filter prime? (range 2 1000000)) 10000))
)

;; https://projecteuler.net/problem=8
;; Largest Product in a Series
(defn largest-product-in-a-series
  "largest product of thirteen adjacent digits in the 1000-digit number: 23514624000"
  []
  (let [_1000-digits "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
       product (fn [s] (reduce * (map #(Integer/parseInt (str %)) (seq s))))]
    (apply max (map #(product (subs _1000-digits % (+ % 13))) (range 0 (- (count _1000-digits) 12))))
  )
)