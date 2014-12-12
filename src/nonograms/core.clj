(ns nonograms.core
  (:gen-class)

  (:require [clojure.core.async :as async
             :refer [close! go >! <! take! put! chan alts!]]
            ;; [clojure.core.async :refer :all :as async]
            [clojure.pprint :refer [pprint]]
            )
  )

;; (:require-macros [cljs.core.async.macros :refer [go]])
;;   (:require [cljs.core.async :as async
;;              :refer [>! <! put! chan alts!]]


;; (go (println "It works!" (<! (go 42))))


;; (defn generate []
;;   (let [fbc (chan 1)]
;;     (go
;;       (>! fbc 1)
;;       (>! fbc 2)
;;       (>! fbc 3)
;;       (>! fbc 4)
;;       (close! fbc))
;;     fbc)
;;   )

;; (def c (generate))

;; (go (println "It works!" (<! c)))
(defn left-most
  "Takes a constraint and returns left most realization as a vector of [pos length] vectors."
  [constraint]
  (reduce (fn [p n]
            (let [last-length (or (last (last p)) 0)
                  last-pos (or (first (last p)) -1)
                  new-pos (+ last-pos last-length 1)]
              (conj p [new-pos n])
              ))
          [] constraint)
  )

(defn move-right-block-by-one
  "Takes a line and moves the right most block one more to the right or returns nil if block is already utmost right."
  [line max-width]
  (let [[pos width] (last line)
        new-end (+ pos width 1)
        ]
    (if (<= new-end max-width) 
      (conj (vec (butlast line)) [(inc pos) width])
      )
    )
  )

(defn process-butlast
  ""
  [partial-constraint max-width]
  (if partial-constraint
    (do
      ;; (println [partial-constraint max-width])
      (permutate partial-constraint max-width)
    )
    )
  )

(concat [[0 1]] (vec (cons [1 2] [])))

(defn permutate [constraint max-width end-blocks]
  (let [line (left-most constraint)]
    (println ">>>" (concat  line end-blocks) )

    (loop  [line (move-right-block-by-one line max-width)]
      (if line
        (let  [partial-constraint (butlast constraint)
               partial-width (dec (first (last line)))
               new-end-blocks (cons  (last line) end-blocks)
               ]
          (if partial-constraint 
            (permutate partial-constraint partial-width new-end-blocks)
            (println "---" (concat line end-blocks) )
            )
          (recur (move-right-block-by-one line max-width))))
      ))
    
  )



(pprint "+++++++++++++++++++++++++++")
(pprint  (permutate [1 2 1] 9 []))
" 
 The idea is that you recursively walk down a tree, with every row of the board being a level in the tree. For every level every possible combination is tried given the constraint for the level. These combinations are the branches of the tree at the level of that row. To avoid having to remember all the combination we're doing a depth first dive into the tree, so at most we remember the number of levels (rows on the board). When we get to the last level we might have found a solution, depending on whether the column constraints are validated. Tro prevent having to try every single combination and having to walk the complete tree we can cut of branches by checking the column constraints for every row combination before we recursively start trying the combinations of the next level. 

"


(def board {
            :rows [[1 2] [4] [1] [2]]
            :columns [ [1] [4] [1] [1]]
            })

(defn height [board] (count (:rows board)))
(defn width [board] (count (:columns board)))


(defn empty-solution [board]
 (repeat (height board) (repeat (width board) nil)) 
  )

(defn test-column
  "Checks a column against its constraint. The column can contain nil for unknown values. Returns the column if it could be validated by the constraint. Nil if it couldn't possibly given the known values in the column"
  [board solution column]

  
  )

(defn next-combination
  "Takes in a contraint and a line width and returns a channel that barfs up all combinations one by one. "
  [constraint width]
  
  )

(defn solve
  "Checks a "
  [board solution level]
  solution
  )



;; (solve board (empty-solution board) 0)




















;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))


;; (def grid {
;;            :size [4 4]
;;            :rows [[1 2] [4] [1] [2]]
;;            :columns [ [1] [4] [1] [1]]
;;            })

;; (def solution [ [0 0 0 1 1 0] [1 1 0 0 1 0]
;;                [ 1 1 1 0 0 1] 
;;             ])

;; (defn transpose-matrix [matrix]
;;   (apply mapv vector matrix))

;; (defn test-row [row]
;;          (->> row
;;            (partition-by identity)
;;            (filter (fn [[x & more]] (= x 1)))
;;            (map count)
;;            ))


;; (defn check [grid solution]
;;   (and
;;    (= (:rows grid) (map test-row solution))
;;    (= (:columns grid) (map test-row (transpose-matrix solution))))
  
;;   )


;; (test-row [0 0 1 1 1 0 1 1 0])


;; (check solution grid)
;; (check grid [[ 1 0] [ 0 1]] )

;; (defn solve-array [l constraint]
;;   (if (= l (+  (apply + constraint) (dec (count constraint))))
;;    (rest  (flatten (map (fn [n]  (conj  (repeat n 1) 0)) constraint)))
;;     (repeat l nil)
;;     )
;;   )

;; (defn compare-places [x y]
;;   (cond (= x y) y
;;         (and (not (nil? x)) (not (nil? y))) :no-way
;;         :default (or x y)
;;         ))

;; ;; (compare-places nil nil)
;; ;; (compare-places 1 nil)
;; ;; (compare-places nil 0)
;; ;; (compare-places 1 0)
;; (map (partial solve-array 5) (:columns grid) )

;; (defn two-v-of-vs [grid]
;;   [(map (partial solve-array (first (:size grid))) (:rows grid) )
;;    (transpose-matrix (map (partial solve-array (second (:size grid))) (:columns grid) ))]
;;   )

;; (defn it-sol [grid]
;;   (apply map (fn [r1 r2] (map compare-places r1 r2)) (two-v-of-vs grid))
;;   )

;; (def first-sol (it-sol grid))

;; (defn index-of-zeros [v]  (map second (filter (fn [[x y]] (= x 0)) (map vector v (range (count v))))))

;; (index-of-zeros [0 1 0])

;; (defn solve-array2 [l constraint partial-sol]
;;   (index-of-zeros (first partial-sol))
;;   )

