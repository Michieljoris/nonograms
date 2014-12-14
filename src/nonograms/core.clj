(ns nonograms.core
  (:gen-class)
  (:require [clojure.core.async :as async
             :refer [pipe close! go >! <! take! put! chan alts!]]
            ;; [clojure.core.async :refer :all :as async]
            [clojure.pprint :refer [pprint]]
            ))

" 
 The idea is that you recursively walk down a tree, with every row of the board being a level in the tree. For every level every possible combination is tried given the constraint for the level. These combinations are the branches of the tree at the level of that row. To avoid having to remember all the combination we're doing a depth first dive into the tree, so at most we remember the number of levels (rows on the board). When we get to the last level we might have found a solution, depending on whether the column constraints are validated. Tro prevent having to try every single combination and having to walk the complete tree we can cut of branches by checking the column constraints for every row combination before we recursively start trying the combinations of the next level. 

"

;; ********** Permutate constraints **********

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

(defn permutate-async [constraint max-width end-blocks]
  "Returns a channel that will keep delivering new permutations of a constraint and nil when all there are no more permutations possible"
  (let [c (chan 1)
        line (left-most constraint)]
    (go
      (>! c (concat  line end-blocks) )
      (loop  [line (move-right-block-by-one line max-width)]
        (if line
          (let  [partial-constraint (butlast constraint)
                 partial-width (dec (first (last line)))
                 new-end-blocks (cons  (last line) end-blocks)]
            (if partial-constraint 
              (let [subc (permutate-async partial-constraint partial-width new-end-blocks)]
                (loop []
                  (when-let [p (<! subc)]
                    (>! c p) (recur))))
              (>! c (concat line end-blocks)))
            (recur (move-right-block-by-one line max-width)))))
      (close! c))
    c))


(defn permutate [constraint max-width end-blocks]
  "Returns a list of all possible permuations of a constraint"
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
          lines)))))

(println "**********")
;; (def result  (permutate [1 2 1 ] 9 []))
;; (doseq [l result] (println l))
;; (def end-blocks [])
;; (def max-width 9)
;; (loop  [line (move-right-block-by-one [[0 1]] max-width)
;;         lines []]
;;         (if line
;;           (recur (move-right-block-by-one line max-width)
;;                  (conj lines (concat line end-blocks) ))
;;           lines))
;; (conj [[[2 3]]] [[1 1]] )

;; ********** Solve **********


(defn height [board] (count (:rows board)))
(defn width [board] (count (:columns board)))

(defn empty-solution [board]
 (repeat (height board) (repeat (width board) nil)) 
  )


(defn test-column
  "Checks a column against its constraint. The column can contain nil for unknown values. Returns the column if it could be validated by the constraint. Nil if it couldn't possibly given the known values in the column"
  [board solution column]
  
  
  )

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
(defn dive-async
  "Finds all combinations of permutations of rows and pushes them one by one by one on the return channel."
  [permutate row  more-rows width]
  (let [c (chan 1)
        permutations (permutate row)]
    (go
      (loop []
        (when-let [p (<! permutations)]
          (if (seq more-rows)
            (let [ch2 (dive-async permutate (first more-rows) (rest more-rows) width)]
              (loop [] 
                (when-let [p2 (<! ch2)]
                  (>! c (cons (line p width) p2))
                  (recur)))
              )
            (>! c [(line p width)])
            )
          (recur)))
      (close! c)
      )
    c
    )
  )

(defn inc-at
  "Increases number in vector v at position n by one. The number flips over to 0 when it's bigger than the base of this position, in this case the :carry tag of the returned tuple is :t. The :indices tag's value is the increased indices vector"
  [v n bases]
  (let [i (inc (nth v n))
        b (nth bases n)]
    ;; (println "in inc-at" v n)
    {:indices (vec (flatten [(subvec v 0 n) (rem i b) (subvec v (inc n))]))
     :carry (if  (= 1 (quot i b)) :t)}))

(inc-at [0 1 4 3 4] 2 [5 5 5 5])

(defn inc-vec
  "Increases the value in indices by one, treating it as a number with a specified base per index. Returns nil if trying to increase max value possible "
  [indices bases]

  (let [l (- (count indices) 1)]
    (loop [ i l
            temp (inc-at indices i bases)
           ]
      ;; (println i temp)
      (if (:carry temp)
        (if (> i 0)
          (recur (dec i) (inc-at (:indices temp) (dec i) bases)))
        (:indices temp)
        )
      ) 
  ))

;; (inc-vec [0 0 0 0] [3 3 3 3])

(defn values-at
  "Uses indices to retrieve and return the values at the corresponding vectors in vectors as a seq"
  [indices vectors]
  (let [fns (map (fn [index] (fn [coll] (nth coll index))) indices)]
    (map (fn [v] ((first v) (second v))) (partition 2 (interleave fns vectors)))
    )
  )

;; (values-at [0 1] [['a 'b] ['c 'd]])


(defn dive
  ""
  [rows width]
  (let [c (chan 1)
        bases (map count rows)
        ]
    (go
      (loop [indices (vec (repeat (count rows) 0))]
        (>! c (map (fn [blocks] (line blocks width))  (values-at indices rows)))
        ;; (>! c (values-at indices rows))
        ;; (>! c indices)
        (if indices
          (recur (inc-vec indices bases)))
        )
      ;; (>! c "Ok then!!")
      )
    c
    )
  )

(defn print-all-solutions
  ""
  [board]
  (let [width (width board)
        permutate (fn [constraint] (permutate constraint width []))
        rows (map permutate  (:rows board))
        solutions (dive rows width)
        ]

    (go
      (loop [result []]
        (let [s (<! solutions)]
          (if s
            (do 
              (println "ok" s)
              (recur (conj result s)))
            result))
        )
      )
    ))

(print-all-solutions board)

;; (cons  '(1 1 3) ['(1 2 3)])
(def board {:rows [ [1 2] [2 2]  ]
;; (def board {:rows [ [1 2] ]
            :columns [ [1] [1] [1] [1] [1] ]})

(defn print-all-solutions-async
  ""
  [board]
  (let [width (width board)
        permutate (fn [constraint] (permutate-async constraint width []))
        rows (:rows board)
        solutions (dive-async permutate (first rows) (rest rows) width)
        ]
    (go
      (println  (loop [result []]
                    (let [s (<! solutions)]
                      (if s
                        (recur (conj result s))
                        result))
                    ))
      )
  ))

(print-all-solutions-async board)


;; (defn perm2 [constraint max-width]
;;   (let [permc (permutate constraint max-width [])]
;;     (go
;;       (loop []
;;         (when-let [p (<! permc)]
;;           (println "ok!!" p)
;;           (recur)
;;           )))
;;       )
;;     )

;; (pprint "+++++++++++++++++++++++++++")
;; (perm2 [1 2] 9)
;; (def c (perm2 [1 2 1] 9))
;; (pprint  (perm2 [1 2 1] 9))

;; (go (println "It works!" (<! c)))
;; (go (println "It works!" (<! c)))


;; (defn empty-solution [board]
;;  (repeat (height board) (repeat (width board) nil)) 
;;   )



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



;; (defn permutate-async2-draft [constraint max-width end-blocks]
;;   "Returns a channel that will keep delivering new permutations of a constraint and nil when all there are no more permutations possible"
;;   (let [line (left-most constraint)
;;         partial-constraint (butlast constraint)
;;         permutations [(concat line end-blocks)]]
;;     (println (concat  line end-blocks) )
;;     (if partial-constraint
;;       (loop  [line (move-right-block-by-one line max-width)]
;;         (if line
;;           (let [
;;                 partial-width (dec (first (last line)))
;;                 new-end-blocks (cons  (last line) end-blocks)
;;                 ]
;;             (permutate partial-constraint partial-width new-end-blocks)
;;             (recur (move-right-block-by-one line max-width)))))

      
;;       (loop  [line (move-right-block-by-one line max-width)]
;;         (if line
;;           (do (println (concat line end-blocks))
;;               (recur (move-right-block-by-one line max-width)))
;;           )))
;;     permutations
;;     ))


