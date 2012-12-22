(ns scad-clj.core
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  ;;(:use [scad-clj.physics])
  (:gen-class :main true))

(defn -main []
  (println "scad-clj")

  (def model
    (let [width 10]
      (defn base [n]
        (hull
         (cylinder 10 20)
         (cylinder [10 30] 20)))
      
      (difference
       (base 3)
       (translate [0 2 0]
         (cylinder width 23)))
      ))

  (write-scad-to-file
   "/home/mfarrell/things/test.scad"
   model)
  )
 
(-main)
