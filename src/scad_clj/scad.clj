(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  (:use [clojure.core.match :only (match)])
  (:use [scad-clj.model])
  (:use [scad-clj.text])
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
;; mesh
(defmethod write-expr :fa [depth [form x]]
  (list (indent depth) "$fa = " x ";\n"))

(defmethod write-expr :fn [depth [form x]]
  (list (indent depth) "$fn = " x ";\n"))

(defmethod write-expr :fs [depth [form x]]
  (list (indent depth) "$fs = " x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defmethod write-expr :cylinder [depth [form {:keys [h r r1 r2 fa fn fs]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (concat `(~(indent depth) "cylinder (" ~fargs "h=" ~h)
            (if (nil? r) (list ", r1=" r1 ", r2=" r2) (list ", r=" r))
            `(", center=true);\n"))))

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere (" fargs "r=" r ", center=true);\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z]}]]
  (list (indent depth) "cube([" x ", " y ", " z "], center=true);\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defmethod write-expr :translate [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "translate ([" x "," y "," z "]) {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotate [depth [form [a [x y z]] & block]]
  (concat
   (list (indent depth) "rotate (a=" (/ (* a 180) Math/PI) ", v=[" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :scale [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "scale ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "mirror ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators
(defmethod write-expr :union [depth [form & block]]
  (concat
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection [depth [form & block]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :hull [depth [form & block]]
  (concat
   (list (indent depth) "hull () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference [depth [form & block]]
  (concat
   (list (indent depth) "difference () {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2d primitives
(defmethod write-expr :square [depth [form {:keys [x y]}]]
  (list (indent depth) "square ([" x ", " y "], center=true);\n"))

(defmethod write-expr :circle [depth [form {:keys [r]}]]
  (list (indent depth) "circle (r = " r ");\n"))

(defmethod write-expr :polygon [depth [form {:keys [points paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(join "],[" (map #(join "," %1) points)) "]]"
    ~@(if (nil? paths) [] `(", paths=[["  ~(join "],[" (map #(join "," %1) paths))  "]]"))
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :projection [depth [form {:keys [cut]} & block]]
  (concat
   (list (indent depth) "projection(cut = " cut ") {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-linear [depth [form {:keys [height twist convexity]} & block]]
  (concat
   (list (indent depth) "linear_extrude(height=" height)
   (if (nil? twist) []  (list ", twist=" twist))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (list ", center=true) {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-rotate [depth [form {:keys [convexity]} & block]]
  (concat
   (list (indent depth) "rotate_extrude(")
   (if (nil? convexity) [] (list "convexity=" convexity))
   (list ") {\n")
   (mapcat #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended
(defmethod write-expr :text [depth [form {:keys [text]}]]
  (defn make-paths [counts]
    (defn foo [prev rst]
      (if (empty? rst) '()
          (cons (range prev (+ prev (first rst)))
                (foo (+ prev (first rst)) (rest rst)))))
    (foo 0 counts))

  (let [polys (text->polygons text)
        points (mapcat identity (mapcat identity polys))
        min-x (apply min (map #(first %1) points))
        min-y (apply min (map #(second %1) points))
        max-x (apply max (map #(first %1) points))
        max-y (apply max (map #(second %1) points))
        ]
    (concat
     (list (indent depth) "translate (["
           (- (/ (- min-x max-x) 2) min-x) ","
           (- (/ (- min-y max-y) 2) min-y) "," 0 "]) {\n")
     (mapcat (fn [letter]
               (concat
                (list (indent (+ 1 depth)) "union () {\n")
                (let [counts (map count letter)
                      verts  (mapcat (fn [x] x) letter)
                      paths  (make-paths counts)]
                  (write-expr (+ depth 2) (polygon verts paths :convexity (count counts))))
                (list (indent (+ 1 depth)) "}\n")))
             polys)
     (list (indent depth) "}\n")
     )))

(defmethod write-expr :extrude-curve [depth [form {:keys [height radius angle n]} & block]]
  (let [lim (Math/floor (/ n 2))]
    (mapcat
     (fn [x]
       (let [theta (* angle (/ x lim) (/ 180 tau))]
         (concat
          (list (indent depth) "rotate(a=" theta ", v=[0,1,0]) {\n")
          (list (indent depth) "linear_extrude(height=" (* 2  height) ", center=true) {\n")
          (mapcat #(write-expr (+ depth 1) %1) block)
          (list (indent depth) "}\n")
          (list (indent depth) "}\n")
          )))
     (range (- lim) (+ lim 1))
     ))
  )
    
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; output
  

(defn write-scad [& block]
  (apply str (write-expr 0 block)))
