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

;; https://projecteuler.net/problem=11
;; Largest Product in a Grid
(def grid [
  ["08" "02" "22" "97" "38" "15" "00" "40" "00" "75" "04" "05" "07" "78" "52" "12" "50" "77" "91" "08"]
  ["49" "49" "99" "40" "17" "81" "18" "57" "60" "87" "17" "40" "98" "43" "69" "48" "04" "56" "62" "00"]
  ["81" "49" "31" "73" "55" "79" "14" "29" "93" "71" "40" "67" "53" "88" "30" "03" "49" "13" "36" "65"]
  ["52" "70" "95" "23" "04" "60" "11" "42" "69" "24" "68" "56" "01" "32" "56" "71" "37" "02" "36" "91"]
  ["22" "31" "16" "71" "51" "67" "63" "89" "41" "92" "36" "54" "22" "40" "40" "28" "66" "33" "13" "80"]
  ["24" "47" "32" "60" "99" "03" "45" "02" "44" "75" "33" "53" "78" "36" "84" "20" "35" "17" "12" "50"]
  ["32" "98" "81" "28" "64" "23" "67" "10" "26" "38" "40" "67" "59" "54" "70" "66" "18" "38" "64" "70"]
  ["67" "26" "20" "68" "02" "62" "12" "20" "95" "63" "94" "39" "63" "08" "40" "91" "66" "49" "94" "21"]
  ["24" "55" "58" "05" "66" "73" "99" "26" "97" "17" "78" "78" "96" "83" "14" "88" "34" "89" "63" "72"]
  ["21" "36" "23" "09" "75" "00" "76" "44" "20" "45" "35" "14" "00" "61" "33" "97" "34" "31" "33" "95"]
  ["78" "17" "53" "28" "22" "75" "31" "67" "15" "94" "03" "80" "04" "62" "16" "14" "09" "53" "56" "92"]
  ["16" "39" "05" "42" "96" "35" "31" "47" "55" "58" "88" "24" "00" "17" "54" "24" "36" "29" "85" "57"]
  ["86" "56" "00" "48" "35" "71" "89" "07" "05" "44" "44" "37" "44" "60" "21" "58" "51" "54" "17" "58"]
  ["19" "80" "81" "68" "05" "94" "47" "69" "28" "73" "92" "13" "86" "52" "17" "77" "04" "89" "55" "40"]
  ["04" "52" "08" "83" "97" "35" "99" "16" "07" "97" "57" "32" "16" "26" "26" "79" "33" "27" "98" "66"]
  ["88" "36" "68" "87" "57" "62" "20" "72" "03" "46" "33" "67" "46" "55" "12" "32" "63" "93" "53" "69"]
  ["04" "42" "16" "73" "38" "25" "39" "11" "24" "94" "72" "18" "08" "46" "29" "32" "40" "62" "76" "36"]
  ["20" "69" "36" "41" "72" "30" "23" "88" "34" "62" "99" "69" "82" "67" "59" "85" "74" "04" "36" "16"]
  ["20" "73" "35" "29" "78" "31" "90" "01" "74" "31" "49" "71" "48" "86" "81" "16" "23" "57" "05" "54"]
  ["01" "70" "54" "71" "83" "51" "54" "69" "16" "92" "33" "48" "61" "43" "52" "01" "89" "19" "67" "48"]
])

;; TODO: refactor
(defn largest-product-in-a-grid
  "greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20x20 grid: 70600674"
  []
  (let [horizontal (for [row (range 0 17) col (range 0 17)] [[row col] [row (+ 1 col)] [row (+ 2 col)]  [row (+ 3 col)]])
        vertical (for [row (range 0 17) col (range 0 17)] [[row col] [(+ 1 row) col] [(+ 2 row) col] [(+ 3 row) col]])
        diagonal (for [row (range 3 20) col (range 0 17)] [[row col] [(- row 1) (+ 1 col)] [(- row 2) (+ 2 col)] [(- row 3) (+ 3 col)]])
        diagonal2 (for [row (range 3 20) col (range 19 2 -1)] [[row col] [(- row 1) (- col 1)] [(- row 2) (- col 2)] [(- row 3) (- col 3)]])
        matrix (fn [points] (for [point (reduce into [] [horizontal vertical diagonal diagonal2])] (map #(get-in grid %) point)))]
    (apply max (for [item (reduce into [] (map matrix [horizontal vertical diagonal diagonal2]))] (reduce #(* %1 (Integer/parseInt %2)) 1 item)))
  )
)


;; https://projecteuler.net/problem=12
;; Highly Divisible Triangular Number
(def triangle-numbers
  (lazy-seq
    (cons 1 (map + (iterate inc 2) triangle-numbers))))

(defn factors
  [n]
  (let [sqrt (int (Math/sqrt n))]
    (reduce into [] (for [i (range 1 (inc sqrt)) :when (= 0 (mod n i))] [i (/ n i)]))))

(defn highly-divisible-triangular-number
  "value of the first triangle number to have over five hundred divisors: 76576500"
  []
  (first (drop-while #(<= (count (factors %)) 500) triangle-numbers))
)