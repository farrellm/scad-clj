(ns scad-clj.designs.text
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  )

(def model
  ;; (text "X O B")
  (extrude-curve {:height 5 :radius 60 :angle (/ tau 4) :n 3}
    (text "X0B"))
  )

(spit "/home/mfarrell/things/clj/text.scad"
      (write-scad model))
