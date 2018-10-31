(ns scad-clj.geometry
  (:require [clojure.core.matrix :refer [distance]]
            [scad-clj.model :refer :all :exclude [import use]]))

(def ^:dynamic *line-radius* 1)

(defn line
  "This function will use all the fancy to draw a line from point A to
  point B."
  [from to]
  (if (= from to)
    (sphere *line-radius*)
    (let [diff (map - to from)
          norm (distance from to)
          rotate-angle (Math/acos (/ (last diff) norm))
          rotate-axis [(- (nth diff 1)) (nth diff 0) 0]]
      (union
       (sphere *line-radius*)
       (translate [0 0 norm]
                  (sphere *line-radius*))
       (->> (cylinder *line-radius* norm)
         (translate [0 0 (/ norm 2)])
         (rotate rotate-angle rotate-axis)
         (translate from))))))

(defn lines [p & ps]
  (apply union
         (:path (reduce (bound-fn [{a :point, path :path} b]
                          {:path (conj path (line a b))
                           :point b})
                        {:point p}
                        ps))))
