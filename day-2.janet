(def grammar
  (let [to-round-struct |(table :red 0 :green 0 :blue 0 ;(reverse $))]
    ~{:main (some :line)
      :line (group (* :game :game-sep :ws (group (some :round)) :line-sep))
      :game (* :ws "Game" :ws :int)
      :round (/ (group (* (some (* :count :count-sep)) :round-sep)) ,to-round-struct)
      :count (* :ws :int :ws :colour) 
      :colour (+ (/ "red" :red) (/ "green" :green) (/ "blue" :blue))
      :int (number :d+)
      :game-sep ":" :count-sep (? ",") :round-sep (? ";") :line-sep (? "\n") 
      :ws (any " ")}))

(defn allowed [{:red r :green g :blue b}] (and (< r 13) (< g 14) (< b 15)))

(defn minimal-bag-power [rounds]
  (let [r-max (max ;(map |($ :red) rounds))
        g-max (max ;(map |($ :green) rounds))
        b-max (max ;(map |($ :blue) rounds))]
    (* r-max g-max b-max)))

(defn sum-allowed-ids [games]
  (sum (seq [[id rs] :in games :when (all allowed rs)] id)))

(defn sum-bag-powers [games]
  (sum (seq [[id rs] :in games] (minimal-bag-power rs))))

(defn main [& args]
  (let [text  (slurp "day-2-input.txt")
        games (peg/match grammar text)]
    (print "step 1: " (sum-allowed-ids games))
    (print "step 2: " (sum-bag-powers games))))

(use judge)
(let [text  ``Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
              Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
              Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
              Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
              Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green``
      games (peg/match grammar text)]
  (test (sum-allowed-ids games) 8)
  (test (sum-bag-powers games) 2286))
