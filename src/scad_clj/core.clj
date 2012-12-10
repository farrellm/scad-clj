(ns scad-clj.core
  (:use [scad-clj.scad])
  (:gen-class :main true))

(defn -main []
  (println "scad-clj")


  (write-scad
   "/home/mfarrell/things/test.scad"
   (constant 'width 18)
   (translate [0 2 0]
              (cylinder :r 'width :h 23))
   (module base [n]
           (cylinder :r 10 :h 20))
   ))
 
(-main)