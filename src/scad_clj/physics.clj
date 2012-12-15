(ns scad-clj.physics
  (:use [scad-clj.scad]))

(defrecord Vector [x y z])
(defrecord Point [px py pz vx vy vz m])
(defrecord Spring [p1 p2 l k])

(defn make-point
  ([x y z] (Point. x y z 0 0 0 1))
  ([x y z m] (Point. x y z 0 0 0 m)))

(defn make-spring
  ([p1 p2 l k] (Spring. p1 p2 l k))
  ([p1 p2 k] (Spring. p1 p2 0 k)))

(defn make-state []
  {:points '() :springs '()})

(defn add-point [state point]
  (assoc state :points (cons (:points state) point)))

(defn add-spring [state spring]
  (assoc state :springs (cons (:springs state) spring)))

(defn render [state file]
  (write-scad file
   (doseq [p (:points state)]
     (translate [(:px p) (:py p) (:pz p)]
                (sphere 1)))
   ))


