(ns scad-clj.model
  (:use [clojure.core.match :only (match)])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mesh
(defn fa! [x]
  `(:fa ~x))

(defn fn! [x]
  `(:fn ~x))

(defn fs! [x]
  `(:fs ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defn cylinder [rs h]
  (match [rs]
    [[r1 r2]] `(:cylinder {:h ~h :r1 ~r1 :r2 ~r2})
    [r] `(:cylinder {:h ~h :r ~r} ~r)))

(defn sphere [r]
  `(:sphere {:r ~r}))

(defn cube [x y z]
  `(:cube {:x ~x :y ~y :z ~z}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))

(defn rotate [a [x y z] & block]
  `(:rotate [~a [~x ~y ~z]] ~@block))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))

(defn mirror [[x y z] & block]
  `(:mirror [~x ~y ~z] ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators

(defn union [ & block]
  `(:union  ~@block))

(defn intersection [ & block]
  `(:intersection  ~@block))

(defn hull [ & block]
  `(:hull  ~@block))

(defn difference [ & block]
  `(:difference  ~@block))