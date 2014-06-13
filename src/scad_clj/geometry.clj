(ns scad-clj.geometry
  (:use [clojure.core.matrix :only [distance]]
        [scad-clj.model]))

(defn line
  "this function will use all the fancy to draw a line from point A to
  point B"
  [from to]
  (let [diff (map - to from)
        norm (distance from to)
        rotate-angle (Math/acos (/ (last diff) norm))
        rotate-axis [(- (nth diff 1)) (nth diff 0) 0]
        ]
    (->> (cylinder 1 norm)
         (translate [0 0 (/ norm 2)])
         (rotate rotate-angle rotate-axis)
         (translate from)
         )))
