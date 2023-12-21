(ns euler.core)

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
