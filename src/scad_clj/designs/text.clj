(ns scad-clj.designs.text
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  (:use [clojure.pprint])
  )

(def model
  ;; (text "X O B")
  (difference
   (translate [0 0 -50]
     (rotate (/ tau 4) [1 0 0]
       (cylinder 80 400)))
   (extrude-curve {:height 20 :radius 70 :angle (* 3 tau) :n 21}
                  (rotate (/ tau 64) [0 0 1]
                    (text "so long, and thanks for all the fishes"))))
  )

;; (pprint model)

(spit "/home/mfarrell/things/clj/text.scad"
      (write-scad model))
