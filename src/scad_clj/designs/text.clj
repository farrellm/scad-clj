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
       (cylinder 60 200)))
   (extrude-curve {:height 12 :radius 50 :angle tau :n 19}
                  (rotate (/ tau 8) [0 0 1]
                    (text "so long, and thanks for all the fishes"))))
  )

;; (pprint model)

(spit "/home/mfarrell/things/clj/text.scad"
      (write-scad model))
