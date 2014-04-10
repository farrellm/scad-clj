(ns scad-clj.geom
  (:use [incanter.stats :only [euclidean-distance]]
        [incanter.core :only [acos]]
        [scad-clj.model]))

(defn draw-line
  "this function will use all the fancy to draw a line from point A to
  point B"
  [from to]
  (let [diff (map - to from)
        norm (euclidean-distance from to)
        rotate-angle (acos (/ (last diff) norm))
        rotate-axis [(- (nth diff 1)) (nth diff 0) 0]
        ]
    (->> (cylinder 1 norm)
         (translate [0 0 (/ norm 2)])
         (rotate rotate-angle rotate-axis)
         (translate from)
         )))
