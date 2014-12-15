(ns nonograms.core
  (:gen-class)
  (:require [clojure.core.async :as async
             :refer [<!! pipe close! go >! <! take! put! chan alts!]]
            ;; [clojure.core.async :refer :all :as async]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as z]
            ))

" 
 The idea is that you recursively walk down a tree, with every row of the board being a level in the tree. For every level every possible combination is tried given the constraint for the level. These combinations are the branches of the tree at the level of that row. To avoid having to remember all the combination we're doing a depth first dive into the tree, so at most we remember the number of levels (rows on the board). When we get to the last level we might have found a solution, depending on whether the column constraints are validated. To prevent having to try every single combination and having to walk the complete tree we can cut of branches by checking the column constraints for every row combination before we recursively start trying the combinations of the next level. 

"

(println "********** Start **********")

; ********** Board **********

(defn height [board] (count (:rows board)))
(defn width [board] (count (:columns board)))

(defn transpose-matrix [matrix]
  (apply mapv vector matrix))

(defn make-constraints
  "Take in a vector of lines and spit out a vector of constraints"
  [grid]
  ;; (println grid)
  (map (fn [line] (->> line
                    (partition-by identity)
                    (filter (fn [[x & more]] (= x 1)))
                    (mapv count)))
       grid))

(defn make-random-board
  "Produce a random board of a certain width and height, density is the percentage of the cells that should be colored in, ranging from 0 to 100"
  [width height density]
  (let [number-of-cells (* width height)
        to-enable (quot (* density number-of-cells) 100)
        all-zeros (repeat number-of-cells 0)
        grid (loop [v (vec all-zeros)
                    enabled 0
                    r (rand-int number-of-cells)
                    ]
               (if (< enabled to-enable)
                 (recur (assoc v r 1)
                        (if (= 0 (nth v r)) (inc enabled) enabled)
                        (rand-int number-of-cells))
                 (partition width v)))]
    { :rows (vec (make-constraints grid))
     :columns (vec (make-constraints (transpose-matrix grid)))
     :grid grid}))


;; (make-random-board 5 5 71)

; ********** Permutate constraints **********

(defn left-most
  "Takes a constraint and returns left most realization as a vector of [pos length] vectors."
  [constraint]
  (reduce (fn [p n]
            (let [last-length (or (last (last p)) 0)
                  last-pos (or (first (last p)) -1)
                  new-pos (+ last-pos last-length 1)]
              (conj p [new-pos n])
              ))
          [] constraint))

(defn move-right-block-by-one
  "Takes a line and moves the right most block one more to the right or returns nil if block is already utmost right."
  [line max-width]
  (let [[pos width] (last line)
        new-end (+ pos width 1)]
    (if (<= new-end max-width) 
      (conj (vec (butlast line)) [(inc pos) width]))))

(defn permutate [constraint max-width end-blocks]
  "Returns a list of all possible permuations of a constraint"
  (if (= 0 (count constraint) )
    [[]]
    (let [line (left-most constraint)
          partial-constraint (butlast constraint)]
      (if partial-constraint
        (loop  [lines [(concat line end-blocks)]
                line (move-right-block-by-one line max-width)]
          (if line
            (let [partial-width (dec (first (last line)))
                  new-end-blocks (cons  (last line) end-blocks)]
              (recur
               (concat lines (permutate partial-constraint partial-width new-end-blocks))
               (move-right-block-by-one line max-width)))
            lines))
        (loop  [lines [(concat line end-blocks)]
                line (move-right-block-by-one line max-width)]
          (if line
            (recur (conj lines (concat line end-blocks) )
                   (move-right-block-by-one line max-width))
            lines))))))

;; (permutate [] 4 [])
;; ********** Solve **********

(defn test-line
  "Checks a line against its constraint. The line can have a tail of nil values. Returns the line if it could be validated by the constraint. Nil if it couldn't possibly given the known values in the line"
  [line constraint]
  (let [partial-constraint
        (->> line
          (partition-by nil?)
          first
          (partition-by identity)
          (filter (fn [[x & more]] (= x 1)))
          (map count))
        test-constraint (take (count partial-constraint) constraint)]
    (if (not (seq constraint))
      (if (seq partial-constraint) nil line)
      ;;constraint has at least one number:
      (if (not (seq partial-constraint))
        line
        ;;both contain one or more numbers:
        (if (> (count partial-constraint) (count constraint))
          nil
          (if (not= (butlast partial-constraint) (butlast test-constraint))
            nil
            ;;compare last numbers:
            (if (<= (last partial-constraint) (last test-constraint))
              line
              nil)))))))

;; nil number ok
;; number nil
;; nil nil ok
;; number number

;; (test-line [0 1 1 nil nil nil] [ 2 1 1 2])

;; (transpose-matrix [['a 'b] ['c 'd]])

(defn test-columns
  "Takes a  board and a (possible partial) solution and returns the solution if it's validated by the boards column constraints"
  [board solution]
  (let [lines (transpose-matrix solution)]
    ;; (println "Test columns" lines)
    (every? (fn [pair] ;;(println pair)
              (apply test-line pair))
            (map vector lines (:columns board)))))

;; (test-columns board [[1 0] [0 1]])

(defn line
  "Takes a line described as vector of blocks and returns a line made up of ones and zeros"
  [blocks width]
  (concat  (reduce (fn [p n]
                     (let [block (repeat (second n) 1)]
                       (let [space-start (count p)
                             space-length (- (first n) space-start)
                             space (repeat space-length 0)]
                         (concat p space block)))
                     ) []  blocks)
           (repeat (- width (apply + (last blocks))) 0)))


;; (concat  (repeat 0 0) [1 2 3])
;; (line [[2 2] [5 5] [11 2]] 13)
;; (line [] 10)

(defn dive
  "Finds all combinations of permutations of rows and pushes them one by one by one on the return channel by walking down the permutations tree of row constraints, ignoring branches that are not validated by the column restraints."
  [rows width solution solution?]
  (let [c (chan 1)]
    (go
      (loop [permutations (first rows)]
        (if (seq permutations)
          (let [p (first permutations)
                solution-line (line p width)
                solution-plus-one (conj solution solution-line)]
            ;; (println solution-plus-one (solution? solution-plus-one))
            (if (or (solution? solution-plus-one) nil)
              (if (seq (rest rows))
                (let [ch2 (dive (rest rows) width solution-plus-one solution?)]
                  (loop [] 
                    (when-let [p2 (<! ch2)]
                      (>! c p2)
                      (recur))))
                (>! c solution-plus-one))
              )
            (recur (rest permutations)))))
      (close! c))
    c))

(defn solve
  "Prints solutions for a nonogram board, limited to max-solutions. All solutions are calculated when max-solutions is nil"
  [board max-solutions]
  (let [width (width board)
        permutate (fn [constraint] (permutate constraint width []))
        rows (map permutate  (:rows board))
        solutions (dive rows width [] (partial test-columns board))]
    (go
    ;; (println "rows" (:rows board) rows)
      (loop [result [] counter 0]
        (let [solution (<! solutions)]
          (if (and solution (or (not max-solutions) (< counter max-solutions)))
            (do 
              ;; (println "ok" solution (test-columns board solution))
              (recur (conj result solution) (inc counter)))
            result))))))

(defn print-grid [grid]
  (println "Grid:")
  (doseq [line grid] (println line)))

(defn test-program [board width height density]
  (let [board (if board board (make-random-board width height density))]
    (println "Board")
    (println "Rows:   " (:rows board))
    (println "Columns:" (:columns board))
    (print-grid (:grid board))
    (go 
      (println "Solutions:" )
      ;; (println (map print-grid (<! (solve board nil))))
      (println (doseq [grid (<! (solve board nil))] (print-grid grid))))
    )
  )

(def board {:rows    [[1] [1 1] [2]]
            :columns [[1] [1] [3]]})

(def board2
  {:size [10 10]
   :rows [[] [1 1] [1] [2 1 1] [1 1 1] [1 2 1 1] [1] [1] [] []]
   :columns [[] [1] [] [3] [1 1] [] [5] [1] [1 4] []]})

(test-program nil 10 10 20)

;; (test-program board2 5 5 20)
