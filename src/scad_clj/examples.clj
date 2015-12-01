(ns scad-clj.examples
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [scad-clj.geometry :refer :all]))

;;; These examples assume you already have openscad installed. If not, follow the
;;; directions in http://www.openscad.org/downloads.html and then come back.


;;; The basic workflow is:
;;;    1. Setup a scene using the functions in scad-clj.model and scad-clj.geometry
;;;    2. Use scad-clj.scad/write-scad to print the scene in valid scad
;;;    3. Spit the output of scad-clj.scad/write-scad to a file
;;;    4. Open the file in openscad to see the result

;;; The basics

;; Built-in shapes

; Cube defined by  x, y, and z sizing
(spit "demo.scad"
      (write-scad (cube 10 10 10)))

; Sphere defined by radius
(spit "demo.scad"
      (write-scad (sphere 10)))

; Cylinder defined by radius and height
(spit "demo.scad"
      (write-scad (cylinder 5 10)))

; Look at each step to see what data you are working with
(cylinder 5 10)
(write-scad (cylinder 5 10))

;; Transformations

; This will be our initial configuration
(->> (cube 2 5 10)
     (write-scad)
     (spit "demo.scad"))

; Let's move it over a bit
(->> (cube 2 5 10)
     (translate [5 0 0]) 
     (write-scad)
     (spit "demo.scad"))

; Let's rotate it
; The first parameter to rotate is the radians to rotate
; the second parameter is the vector to rotate around
(->> (cube 2 5 10)
     (rotate (/ pi 4) [0 1 0])
     (write-scad)
     (spit "demo.scad"))

; Now let's make it bigger
(->> (cube 2 5 10)
     (scale [10 1 1])
     (write-scad)
     (spit "demo.scad"))

; We can combine two (or more) shapes
(->> [(cylinder 1 4 ) (sphere 2)]
     (union)
     (write-scad)
     (spit "demo.scad"))
