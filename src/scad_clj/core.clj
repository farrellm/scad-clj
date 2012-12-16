(ns scad-clj.core
  (:use [scad-clj.scad])
  (:use [scad-clj.physics])
  (:gen-class :main true))

(defn -main []
  (println "scad-clj")


  (write-scad
   "/home/mfarrell/things/test.scad"
   (constant width 18)
   (translate [0 2 0]
              (cylinder width 23))
   (module base [n]
     (cylinder 10 20)
     ;;(cylinder 10 :r2 30 :h 20)
     ))

  ;; (let [state (ref (make-state))]
  ;;   (alter z)
  ;;   (render @state "/home/mfarrell/things/physics.scad")
  ;;   )
  )
 
(-main)