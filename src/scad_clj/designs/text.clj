(ns scad-clj.designs.text
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  (:use [clojure.pprint])
  )

(def model
  ;; (difference
  ;; (intersection
  (union
   (cylinder 200 500)
   
   (rotate (/ tau 4) [1 0 0]
     (translate [0 0 200]
       (let [m 3]
         (extrude-curve {:height 20 :radius 200 :angle (* m tau) :n (* m 29)}
                        ;; (extrude-linear {:height 20}
                        (rotate (/ tau 64) [0 0 1]
                          (text {:face :script}
                                "so long, and thanks for all the fishes"))
                        ))))))

(pprint model)

(spit "/home/mfarrell/things/clj/text.scad"
      (write-scad model))
