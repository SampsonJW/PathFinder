; Name: Sampson Ward
; ID:   1312744

(ns assignment2.core
  (:use clojure.pprint)
  (:require [clojure.java.io :as io])
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
)


(defn read-map-from-file
  "Takes a filename and reads a map in text format,
  A newly created state with a zero length path is returned by this function"
  [itsTheMap]

  {:map (str/split-lines (slurp (io/reader itsTheMap))) :path[]} ;slurp in map and split line by line, put into hashmap

)


(defn search-map
  "loop through each position in hashmap and compare characters"
  [state c]
  (let [daMap (:map state)]
  (first
  (for [y (range(count daMap))           ;make y the range of the collumns on map
        x (range(count(first daMap)))    ;make x the range of the rows in map
        :let[char_ (get(get daMap y) x)] ;make character that of char at position
        :when(= char_ c)] [x y])))       ;when the character matches that of the supplied in params then return position
)


(defn start
  "Find the starting point in the map"
  [state]
  (search-map state \S)      ;search map for start position
)


(defn goal
  "Find the starting point in the map"
  [state]
  (search-map state \G)      ;search map for goal position
)


(defn cost
  "finds the cost of the path"
  [state]
  (count (:path state))     ;count items in the path key of the state
)


(defn get-positions
  "which takes a state and returns the coordinates of the ship on the map,
  which can be worked out by following the path from the start(println (apply str pMap))."
  [state]

  (loop [poss [(start state)] path (:path state)]
    (if (empty? path) poss              ;if the path is empty then return the current position
      (let [[x y] (last poss)           ;let the xy equal the last position (current) in vector
            newpos (case (first path)   ;let new position equal that which the path points to
                     :south [x (inc y)]
                     :north [x (dec y)]
                     :east [(inc x) y]
                     :west [(dec x) y])]
        (recur (conj poss newpos) (rest path)))))
  )

(defn position
  "which takes a state and returns the coordinates of the ship on the map,
   which can be worked out by following the path from the start."
  [state]
  (last(get-positions state))
)



(defn print-state
  "Pretty print map to the console"
  [state]
  (loop [daMap (mapv vec (:map state))   ;let daMap equal the :map of the parsed state
         posits (get-positions state)]   ;get all positions in the states path
    (if (empty? posits)                  ;if there is no path yet
      (doseq [pMap daMap] (println (apply str pMap))) ;print map to console
      (let [[x y] (first posits)]        ;else get the first position
        (recur (if (= (get-in daMap [y x]) \space) (assoc-in daMap [y x] \.) daMap) ;if the path has a space then fill with a dot
               (rest posits)))))         ;do the same for the rest of the path
  )


(defn heuristic
  "takes a state and computes its heuristic value using the Euclidean distance metric"
  [state]

  (let [[Px Py] (position state)
        [Gx Gy] (goal state)
        xDif (- Px Gx)
        yDif (- Py Gy)]

    (Math/sqrt (+ (* xDif xDif) (* yDif yDif))))
  )



(defn expand
  "which takes a state returns a list of new states obtained by extending the
  length  of  the  given  state’s  path  by  one  in  all  possible  valid  directions"
  [state]
  (let [posits (get-positions state)
        [x y] (last posits)
        daMap (:map state)
        goodChars [\G \space]
        ]

    (remove nil?                                         ;removes nil positions
            (map(fn [direc]                              ;maps directions to direc
                  (let [[newX newY] (case direc          ;let new x and y equal that of which pos is true
                                      :south [x (inc y)] ;increment/decrement x and y depending on direction in path
                                      :north [x (dec y)]
                                      :east  [(inc x) y]
                                      :west  [(dec x) y])
                        chr (get (get daMap newY) newX)] ;get charachter from position on the map
                    (if (and (or (= chr \space) (= chr \G)) (not (.contains posits [newX newY]))) ;if the character is a empty block or the goal and if position is not a previous position
                      {:map daMap, :path (conj (:path state) direc)}))) ;add new direction to path
                '(:north :east :south :west))))
  )

(def verbose false)


(defn best-first
  ""
  [fileName]
  (let [state (read-map-from-file fileName)]
    (loop[p-map (priority-map state (heuristic state)) counter 0 paths-vec #{}]              ;makes a priority map of a state and the heuristic, a counter and a set of visited positions
      (let [currState (first (peek p-map))]                                                  ;current state is the state with the lowest heuristic value (first item in the proirity map)
        (if (= (position currState) (goal state))                                            ;if the position is at the goal
          (do(print-state currState) (println (:path currState))(println "Expansions:"counter) (println "Cost:"(cost currState))) ;print
          (do
            (if verbose (print-state currState))                                             ;if verbose is set then print states
            (recur (into (pop p-map)                                                         ;pop the last state and heuristic
                         (for [st (expand currState)                                         ;for each state expand again
                               :let [her (heuristic st)]                                     ;let her be the heuristics of expanded states
                               :when (not (contains? paths-vec (position st)))] [st her]))   ;when the position does not equal that of one in the visited paths set return the new state and heuristic
                   (inc counter) (into paths-vec (for[exp-states(expand currState)]          ;else for each expanded state get the positions and store in set
                                                   (position exp-states)))))))))
  )

(defn a-star
  ""
  [fileName]
  (let [state (read-map-from-file fileName)]
    (loop[p-map (priority-map state (+(heuristic state) (cost state))) counter 0 paths-vec #{}]
      (let [currState (first (peek p-map))]
        (if (= (position currState) (goal state))
          (do(print-state currState) (println (:path currState))(println "Expansions:"counter) (println "Cost:"(cost currState)))
          (do
            (if verbose (print-state currState))
            (recur (into (pop p-map)
                         (for [st (expand currState)
                               :let [her (+ (heuristic st) (cost st))]         ;in this case add cost and heuristic to her
                               :when (not (contains? paths-vec (position st)))]
                           [st her])) (inc counter) (into paths-vec (for[exp-states(expand currState)]
                                                                      (position exp-states)))))))))
)
