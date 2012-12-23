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

(def ^:dynamic *fa* false)
(defmacro with-fa [x & block]
  `(binding [*fa* ~x]
     (list ~@block)))

(def ^:dynamic *fn* false)
(defmacro with-fn [x & block]
  `(binding [*fn* ~x]
     (list ~@block)))

(def ^:dynamic *fs* false)
(defmacro with-fs [x & block]
  `(binding [*fs* ~x]
     (list ~@block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defn cylinder [rs h]
  (let [fargs (merge (if *fa* {:fa *fa*} {})
                     (if *fn* {:fn *fn*} {})
                     (if *fs* {:fs *fs*} {}))]
    (match [rs]
      [[r1 r2]] `(:cylinder ~(merge fargs {:h h :r1 r1 :r2 r2}))
      [r] `(:cylinder ~(merge fargs {:h h :r r})))))

(defn sphere [r]
  (let [args (merge {:r r}
                    (if *fa* {:fa *fa*} {})
                    (if *fn* {:fn *fn*} {})
                    (if *fs* {:fs *fs*} {}))]
    `(:sphere ~args)))

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