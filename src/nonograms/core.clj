(ns nonograms.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def grid {
           :size [4 4]
           :rows [[1 2] [4] [1] [2]]
           :columns [ [1] [4] [1] [1]]
           })

(def solution [ [0 0 0 1 1 0] [1 1 0 0 1 0]
               [ 1 1 1 0 0 1] 
            ])

(defn transpose-matrix [matrix]
  (apply mapv vector matrix))

(defn test-row [row]
         (->> row
           (partition-by identity)
           (filter (fn [[x & more]] (= x 1)))
           (map count)
           ))


(defn check [grid solution]
  (and
   (= (:rows grid) (map test-row solution))
   (= (:columns grid) (map test-row (transpose-matrix solution))))
  
  )


(test-row [0 0 1 1 1 0 1 1 0])


(check solution grid)
(check grid [[ 1 0] [ 0 1]] )

(defn solve-array [l constraint]
  (if (= l (+  (apply + constraint) (dec (count constraint))))
   (rest  (flatten (map (fn [n]  (conj  (repeat n 1) 0)) constraint)))
    (repeat l nil)
    )
  )

(defn compare-places [x y]
  (cond (= x y) y
        (and (not (nil? x)) (not (nil? y))) :no-way
        :default (or x y)
        ))

;; (compare-places nil nil)
;; (compare-places 1 nil)
;; (compare-places nil 0)
;; (compare-places 1 0)
(map (partial solve-array 5) (:columns grid) )

(defn two-v-of-vs [grid]
  [(map (partial solve-array (first (:size grid))) (:rows grid) )
   (transpose-matrix (map (partial solve-array (second (:size grid))) (:columns grid) ))]
  )

(defn it-sol [grid]
  (apply map (fn [r1 r2] (map compare-places r1 r2)) (two-v-of-vs grid))
  )

(def first-sol (it-sol grid))

(defn index-of-zeros [v]  (map second (filter (fn [[x y]] (= x 0)) (map vector v (range (count v))))))

(index-of-zeros [0 1 0])

(defn solve-array2 [l constraint partial-sol]
  (index-of-zeros (first partial-sol))
  )























