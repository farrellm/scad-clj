(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  (:use [clojure.core.match :only (match)])
  (:use [scad-clj.model])
  (:use [clojure.pprint])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethod

(defmulti write-expr
  (fn [depth [form & args]]
    (if (keyword? form) form :list)))

(defmethod write-expr :default [depth [form & args]]
  `("//(" ~form ~args ")"))

(defmethod write-expr :list [depth [& args]]
  (mapcat #(write-expr depth %1) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defn indent [depth]
  (apply str (repeat depth "  ")))

(defn write-block [depth block]
  (mapcat #(write-expr (+ depth 1) %1) block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include and call into scad libraries.

(declare map-to-arg-string)

(defn make-arguments [args]
  (let [arg (first args)
        rest (rest args)
        piece (cond
               (map? arg) (map-to-arg-string arg)
               (coll? arg) (str "[" (make-arguments arg) "]")
               :else arg)]
    (if (empty? rest)
      piece
      (join ", " [piece (make-arguments rest)]))))

(defn map-to-arg-string [m]
  (join ", " (map (fn [[k v]] (str (name k) "=" (make-arguments [v])) ) m)))

(defmethod write-expr :include [depth [form {:keys [library]}]]
  (list (indent depth) "include <" library">\n"  ))

(defmethod write-expr :call [depth [form {:keys [function]} & args]]
  (list (indent depth) function  "(" (make-arguments (apply vec args)) ");\n"  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defmethod write-expr :circle [depth [form {:keys [r]}]]
  (list (indent depth) "circle (r = " r ");\n"))

(defmethod write-expr :square [depth [form {:keys [x y]}]]
  (list (indent depth) "square ([" x ", " y "], center=true);\n"))

(defmethod write-expr :polygon [depth [form {:keys [points paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]]"
    ~@(if (nil? paths) [] `(", paths=[["  ~(join "], [" (map #(join "," %1) paths))  "]]"))
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere (" fargs "r=" r ", center=true);\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z]}]]
  (list (indent depth) "cube ([" x ", " y ", " z "], center=true);\n"))

(defmethod write-expr :cylinder [depth [form {:keys [h r r1 r2 fa fn fs]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (concat `(~(indent depth) "cylinder (" ~fargs "h=" ~h)
            (if (nil? r) (list ", r1=" r1 ", r2=" r2) (list ", r=" r))
            `(", center=true);\n"))))

(defmethod write-expr :polyhedron [depth [form {:keys [points faces convexity]}]]
  `(~@(indent depth) "polyhedron ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]], "
    "faces=[[" ~(join "], [" (map #(join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defmethod write-expr :translate [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "translate ([" x ", " y ", " z "]) {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotate [depth [form [a [x y z]] & block]]
  (concat
   (list (indent depth) "rotate (a=" (/ (* a 180) pi) ", v=[" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :scale [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "scale ([" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "mirror ([" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :color [depth [form [r g b a] & block]]
  (concat
    (list (indent depth) "color ([" r ", " g ", " b ", " a"]) {\n")
    (write-block depth block)
    (list (indent depth) "}\n")))

(defmethod write-expr :hull [depth [form & block]]
  (concat
   (list (indent depth) "hull () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :minkowski [depth [form & block]]
  (concat
   (list (indent depth) "minkowski () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean operations

(defmethod write-expr :union [depth [form & block]]
  (concat
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference [depth [form & block]]
  (concat
   (list (indent depth) "difference () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection [depth [form & block]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defmethod write-expr :projection [depth [form {:keys [cut]} & block]]
  (concat
   (list (indent depth) "projection (cut = " cut ") {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-linear [depth [form {:keys [height twist convexity]} & block]]
  (concat
   (list (indent depth) "linear_extrude (height=" height)
   (if (nil? twist) [] (list ", twist=" twist))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (list ", center=true) {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-rotate [depth [form {:keys [convexity]} & block]]
  (concat
   (list (indent depth) "rotate_extrude (")
   (if (nil? convexity) [] (list "convexity=" convexity))
   (list ") {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :render [depth [form & block]]
  (concat
   (list (indent depth) "render () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables

(defmethod write-expr :fa [depth [form x]]
  (list (indent depth) "$fa = " x ";\n"))

(defmethod write-expr :fn [depth [form x]]
  (list (indent depth) "$fn = " x ";\n"))

(defmethod write-expr :fs [depth [form x]]
  (list (indent depth) "$fs = " x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

(defn write-scad [& block]
  (apply str (write-expr 0 block)))
