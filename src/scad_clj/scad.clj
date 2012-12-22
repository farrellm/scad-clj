(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  (:use [clojure.core.match :only (match)])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forward declarations
(declare write-expr)
(declare write-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
(defn indent [depth]
  (apply str (repeat depth "  ")))

(defn write-block [depth block]
  (flatten (map #(write-expr (+ depth 1) %1) block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mesh
(defn write-fa [depth x]
  (list (indent depth) "$fa = " x ";\n"))

(defn write-fn [depth x]
  (list (indent depth) "$fn = " x ";\n"))

(defn write-fs [depth x]
  (list (indent depth) "$fs = " x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defn write-cylinder [depth [{h :h r :r r1 :r1 r2 :r2}]]
  (if (nil? r)
    (list (indent depth) "cylinder (h=" h ", r1=" r1 ", r2=" r2 ", center=true);\n")
    (list (indent depth) "cylinder (h=" h ", r=" r ", center=true);\n")))

(defn write-sphere [depth [{r :r}]]
  (list (indent depth) "sphere (r=" r ", center=true);\n"))

(defn write-cube [depth [{x :x y :y z :z}]]
  (list (indent depth) "cube(x=" x ", y=" y ", z=" z ", center=true);\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defn write-translate [depth [[x y z] & block]]
  (list
   (list (indent depth) "translate ([" x "," y "," z "]) {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn write-rotate [depth [[a [x y z]] & block]]
  (list
   (list (indent depth) "rotate (a=" (/ (* a 180) Math/PI) ", v=[" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn write-scale [depth [[x y z] & block]]
  (list
   (list (indent depth) "scale ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn write-mirror [depth [[x y z] & block]]
  (list
   (list (indent depth) "mirror ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators
(defn write-union [depth [ & block]]
  (list
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn write-intersection [depth [ & block]]
  (list
   (list (indent depth) "intersection () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn write-hull [depth [ & block]]
  (list
   (list (indent depth) "hull () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn write-difference [depth [ & block]]
  (list
   (list (indent depth) "difference () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def form-writer
  {
   :fa write-fa 
   :fn write-fn 
   :fs write-fs 

   :cylinder write-cylinder
   :sphere write-sphere
   :cube write-cube

   :translate write-translate
   :rotate write-rotate
   :scale write-scale
   :mirror write-mirror

   :union write-union
   :intersection write-intersection
   :hull write-hull
   :difference write-difference
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output
(defn write-expr [depth [form & args]]
  (if (keyword? form)
    (apply (get form-writer form) (list depth args))
    (write-list depth (cons form args))
    ))

(defn write-list [depth list]
  (map #(write-expr depth %1) list))

(defn write-scad [& block]
  (apply str (flatten (write-list 0 block))))

(defn write-scad-to-file [path & block]
  (with-open [wrtr (writer path)]
    (.write wrtr (write-scad block))))
