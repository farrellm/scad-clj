(ns scad-clj.designs.sphere
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  )

(def model
  (let [n 3
        radius 10
        pillar 2
        gap 0.1]
    (with-fn 100
      (translate [0 0 radius]
        (sphere radius))))
   )

(write-scad-to-file "/home/mfarrell/things/clj/sphere.scad" model)
