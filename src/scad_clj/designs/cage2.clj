(ns scad-clj.designs.cage
  (:use [scad-clj.scad])
  (:use [scad-clj.model]))

(def tau (* 2 Math/PI))

(def model
  (union
   (union
    (extrude-rotate (translate [7 0 0] (circle 2)))
    (extrude-linear {:height 10 :twist 90}
      (square 4 4))
    (extrude-linear {:height 30 :twist 90}
      (polygon [[0,0], [10,0], [0,10]], [[0,1,2]] :convexity 1))
    )))

(spit "/home/mfarrell/things/clj/cage2.scad"
      (write-scad model))

model
