(ns scad-clj.scad
  (:require [clojure.string :refer [join]]
            [scad-clj.model :refer [rad->deg]]))

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
  (join (repeat depth "  ")))

(defn write-block [depth block]
  (mapcat #(write-expr (inc depth) %1) block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier

(defmethod write-expr :modifier [depth [form modifier & block]]
  (concat
   (list (indent depth) modifier "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

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
  (list (indent depth) "include <" library">\n"))

(defmethod write-expr :use [depth [form {:keys [library]}]]
  (list (indent depth) "use <" library">\n"))

(defmethod write-expr :import [depth [form file]]
  (list (indent depth) "import (\"" file "\");\n"))

(defmethod write-expr :call [depth [form {:keys [function]} & args]]
  (list (indent depth) function "(" (make-arguments (apply vec args)) ");\n"))

(defmethod write-expr :call-module-with-block [depth [form {:keys [module]} & args]]
  (let [the-args (butlast (first args))
        block (list (last (first args)))]
    (concat
     (list (indent depth) module " (" (make-arguments (vec the-args)) ") {\n")
     (write-block depth block)
     (list (indent depth) "}\n"))))

(defmethod write-expr :call-module-no-block [depth [form {:keys [module]} & args]]
  (let [the-args (first args)]
    (list (indent depth) module " (" (make-arguments (vec the-args)) ");\n")))

(defmethod write-expr :define-module [depth [form {:keys [module]} & args]]
  (let [the-args (butlast (first args))
        block (list (last (first args)))]
    (concat
     (list (indent depth) "module " module "(" (make-arguments (vec the-args)) ") {\n")
     (write-block depth block)
     (list (indent depth) "};\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defmethod write-expr :circle [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "circle (" fargs "r=" r ");\n")))

(defmethod write-expr :square [depth [form {:keys [x y center]}]]
  (list (indent depth) "square ([" x ", " y "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :polygon [depth [form {:keys [points paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]]"
    ~@(when paths [", paths=[[" (join "], [" (map #(join "," %1) paths)) "]]"])
    ~@(when convexity [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :text [depth [form {:keys [text size font halign valign spacing direction language script fn]}]]
  (list (indent depth) "text (\"" text "\""
        (when fn (str ", $fn=" fn))
        (when size (str ", size=" size))
        (when font (str ", font=\"" font "\""))
        (when halign (str ", halign=\"" halign "\""))
        (when valign (str ", valign=\"" valign "\""))
        (when spacing (str ", spacing=" spacing))
        (when direction (str ", direction=\"" direction "\""))
        (when language (str ", language=\"" language "\""))
        (when script (str ", script=\"" script "\""))");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere (" fargs "r=" r ");\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z center]}]]
  (list (indent depth) "cube ([" x ", " y ", " z "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :cylinder [depth [form {:keys [h r r1 r2 fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (concat
     (list (indent depth) "cylinder (" fargs "h=" h)
     (if r (list ", r=" r) (list ", r1=" r1 ", r2=" r2))
     (when center (list ", center=true"))
     (list ");\n"))))

(defmethod write-expr :polyhedron [depth [form {:keys [points faces convexity]}]]
  `(~@(indent depth) "polyhedron ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]], "
    "faces=[[" ~(join "], [" (map #(join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defmethod write-expr :resize [depth [form {:keys [x y z auto]} & block]]
  (concat
   (list (indent depth) "resize ([" x ", " y ", " z "]")
   (list (when-not (nil? auto)
           (str " auto="
                (if (coll? auto)
                  (str "[" (join ", " (map true? auto)) "]")
                  (true? auto)))))
   "){\n"
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :translate [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "translate ([" x ", " y ", " z "]) {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotatev [depth [form [a [x y z]] & block]]
  (concat
   (list (indent depth) "rotate (a=" (rad->deg a) ", v=[" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotatec [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "rotate ([" (rad->deg x) "," (rad->deg y) "," (rad->deg z) "]) {\n")
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
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :offset
  [depth [form {:keys [r delta chamfer] :or {chamfer false}} & block]]
  (concat
   (list (indent depth) "offset (")
   (if r
     (list "r = " r)
     (list "delta = " delta))
   (when chamfer (list ", chamfer=true"))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :minkowski [depth [form & block]]
  (concat
   (list (indent depth) "minkowski () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :multmatrix [depth [form m & block]]
  (let [w (fn [s] (str "[" s "]")) ;; wrap
        co (fn [c] (apply str (interpose "," c)))] ;; put commas in between
    (concat
     (list (indent depth) "multmatrix(")
     (w (co (map #(w (co %)) m)))
     (list ") {\n")
     (mapcat #(write-expr (inc depth) %1) block)
     (list (indent depth) "}\n"))))

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
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection [depth [form & block]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defmethod write-expr :surface [depth [form {:keys [filepath convexity center invert]}]]
  (concat
   (list (indent depth) "surface (file = \"" filepath "\""
         (when convexity (format ", convexity=%d" convexity))
         (when center ", center=true")
         (when invert ", invert=true")
         ");\n")))

(defmethod write-expr :projection [depth [form {:keys [cut]} & block]]
  (concat
   (list (indent depth) "projection (cut = " cut ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-linear [depth [form {:keys [height twist convexity center slices scale]} & block]]
  (concat
   (list (indent depth) "linear_extrude (height=" height)
   (if (nil? twist) [] (list ", twist=" (rad->deg twist)))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (if (nil? slices) [] (list ", slices=" slices))
   (cond
     (nil? scale) []
     (sequential? scale) (list ", scale=[" (first scale) ", " (second scale) "]")
     :else (list ", scale=" scale))
   (when center (list ", center=true"))
   (list "){\n")

   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-rotate [depth [form {:keys [convexity fn angle]} & block]]
  (concat
   (list (indent depth) "rotate_extrude (")
   (join ", "
     (concat
       (if convexity [(str "convexity=" convexity)])
       (if angle [(str "angle=" angle)])
       (if fn [(str "$fn=" fn)])))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :render [depth [form {:keys [convexity]} & block]]
  (concat
   (list (indent depth) (str "render (convexity=" convexity ") {\n"))
   (mapcat #(write-expr (inc depth) %1) block)
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
  (join (write-expr 0 block)))
