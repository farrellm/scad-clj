(ns scad-clj.designs.cycloid
  (:use [scad-clj.physics])
  (:use [scad-clj.model])
  )

(def model
  ;; (cube 1 2 3)
  (translate [50 0 50]
    (union
     (sphere 10)
     (translate [0 0 100]
       (cube 20 10 10))))
  )

(render model)