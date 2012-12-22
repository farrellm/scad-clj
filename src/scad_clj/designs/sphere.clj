(ns scad-clj.designs.cage
  (:use [scad-clj.scad]))

(def model
  (list
   (fn! 100)
   (let [n 3
         radius 10
         pillar 2
         gap 0.1]
     (translate [0 0 radius]
       (sphere radius)))
   ))

(with-open [wrtr (writer "/home/mfarrell/things/clj/sphere.scad")]
  (.write wrtr (write-scad ast)))