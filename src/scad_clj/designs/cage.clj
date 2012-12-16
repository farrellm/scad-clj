(ns scad-clj.designs.cage
  (:use [scad-clj.scad]))

(write-scad
 "/home/mfarrell/things/clj/cage.scad"

 (constant radius 10)
 (constant height radius)

 (sphere radius)
 (cylinder radius 9)
 (module mod [r]
   (sphere r)
   (cylinder 4 2)
   )
 (mod 9)
 )

