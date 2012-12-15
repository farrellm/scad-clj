(ns scad-clj.designs.cage
  (:use [scad-clj.scad]))

(write-scad
   "/home/mfarrell/things/clj/cage.scad"
   (constant width 18)
   (constant height width)
   (sphere :r width)
   (cylinder :r width :h 9)
   (module mod [r]
     (sphere :r r)
     (cylinder :r 4 :h 2)
     )
   (mod 9)
   )

