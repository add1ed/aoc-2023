(defn build-number [digits]
  (let [tens (first digits)
        ones (last digits)]
    (-> tens (* 10) (+ ones))))

(def words 
  (let [numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
        to-pattern (fn [[idx word]] ~(/ (if ,word 1) ,idx))]
    (map to-pattern (pairs numbers))))

(defn grammar [extras]
  ~{:main (some :line)
    :line (* (/ (group (some :digit)) ,build-number) (? "\n"))
    :digit (+ ,;extras (number :d) :a)})

(defn main [& args]
  (let [text   (slurp "day-1-input.txt")
        step-1 (sum (peg/match (grammar []) text))
        step-2 (sum (peg/match (grammar words) text))]
    (print "step 1: " step-1)
    (print "step 2: " step-2)))

(use judge)
(defn extract [str] (get (peg/match (grammar []) str) 0))
(defn extract-2 [str] (get (peg/match (grammar words) str) 0))

(test (extract "1abc2") 12)
(test (extract "pqr3stu8vwx") 38)
(test (extract "a1b2c3d4e5f") 15)
(test (extract "treb7uchet") 77)

(test (extract-2 "1abc2") 12)
(test (extract-2 "pqr3stu8vwx") 38)
(test (extract-2 "a1b2c3d4e5f") 15)
(test (extract-2 "treb7uchet") 77)
(test (extract-2 "two1nine") 29)
(test (extract-2 "eightwothree") 83)
(test (extract-2 "abcone2threexyz") 13)
(test (extract-2 "xtwone3four") 24)
(test (extract-2 "4nineeightseven2") 42)
(test (extract-2 "zoneight234") 14)
(test (extract-2 "7pqrstsixteen") 76)

