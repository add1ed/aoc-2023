(defn to-range-size [[t d]]
  # quadratic equation is x^2 + t*x - d.
  (let [disc (math/sqrt (- (* t t) (* 4 d)))
        roots [(/ (- t disc) 2) (/ (+ t disc) 2)]
        size (+ (- (math/floor (last roots)) (math/ceil (first roots))) 1)]
    (if (int? disc) (- size 2) size)))

(defn to-range-product [records] (->> records (map to-range-size) product))
(defn zip [l r] (pairs (zipcoll l r)))

(def grammar-1
  ~{:main (/ (* :times  :s+ :distances) ,zip)
    :times (* "Time:" (group (some :number)))
    :distances (* "Distance:" (group (some :number)))
    :number (* :ws (number :d+))
    :ws (any " ")})

(def grammar-2
  ~{:main (/ (* :times  :s+ :distances) ,zip)
    :times (* "Time:" (group :concat-number))
    :distances (* "Distance:" (group :concat-number))
    :concat-number (/ (% (some :number)) ,scan-number)
    :number (* :ws (number :d+))
    :ws (any " ")})

(defn main [& args]
  (let [text      (slurp "day-6-input.txt")
        [races-1] (peg/match grammar-1 text)
        [races-2] (peg/match grammar-2 text)]
    (print "step 1:" (to-range-product races-1))
    (print "step 2:" (to-range-product races-2))))

(use judge)
(let [text      ``Time:      7  15   30
                  Distance:  9  40  200``
      [races-1] (peg/match grammar-1 text)
      [races-2] (peg/match grammar-2 text)]
 (test (to-range-product races-1) 288)
 (test (to-range-product races-2) 71503))
