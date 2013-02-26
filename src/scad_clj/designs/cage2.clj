(ns scad-clj.designs.cage
  (:use [scad-clj.scad])
  (:use [scad-clj.model]))

(def tau (* 2 Math/PI))

(def model
  (project
   (union
    (extrude-rotate (translate [7 0 0] (circle 2)))
    (extrude-linear {:height 10 :twist 90}
      (square 4 4))))
  )

(spit "/home/mfarrell/things/clj/cage2.scad"
      (write-scad model))

model
