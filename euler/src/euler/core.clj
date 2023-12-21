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