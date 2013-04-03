(ns scad-clj.designs.text
  (:use [scad-clj.scad])
  (:use [scad-clj.model])
  (:use [clojure.pprint])
  )

(def model
  (difference
   ;; (intersection
   (union
    (with-fn 100
      (cylinder 200 500))

    (translate [0 0 250]
      (extrude-linear {:height 30}
        (text "42" :size 18))))
   
   (render
    (rotate (/ tau 4) [1 0 0]
      (translate [0 0 200]
        (let [m 3]
          (extrude-curve {:height 30 :radius 200 :angle (* m tau) :n (* m 29)}
                         ;; (extrude-linear {:height 20}
                         (rotate (/ tau -64) [0 0 1]
                           (scale [0.7 0.7 0.7]
                             (text "so long, and thanks for all the fishes"
                                   :face :script :size 10.0))))))))))

;; (pprint model)

(spit "/home/mfarrell/things/clj/text.scad"
      (write-scad model))
