(def grammar
  ~{:main (some :line)
    :line (group (* (any (+ :s :w)) :card-sep :ws :numbers :num-sep :ws :numbers :line-sep))
    :numbers (group (some (* (number :d+) :ws)))
    :card-sep ":" :num-sep "|" :line-sep (? "\n") :ws (any " ")})

(defn wins [[winning ns]] (count |(has-value? winning $) ns))
(defn score [wins] (math/floor (math/pow 2 (- wins 1))))
(defmacro add-within [ds from to] ~(put ,ds ,to (+ (get ,ds ,to) (get ,ds ,from))))
(defn sum-card-scores [cards] (sum (->> cards (map wins) (map score))))

(defn count-all-copies [cards]
  (let [len    (length cards)
        counts (array/new-filled len 1)]
    (loop [[idx card] :pairs cards
           i :range-to [1 (wins card)]
           :let [pos (+ idx i)]
           :when (< pos len)]
      (add-within counts idx pos)) 
    (sum counts)))

(defn main [& args]
  (let [text  (slurp "day-4-input.txt")
        cards (peg/match grammar text)]
    (print "step 1:" (sum-card-scores cards))
    (print "step 2:" (count-all-copies cards))))

(use judge)
(let [text ``Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
             Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
             Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
             Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
             Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
             Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11``
      cards (peg/match grammar text)]
  (test (sum-card-scores cards) 13)
  (test (count-all-copies cards) 30))
