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
;; constants
(defmacro constant [name value]
  `(do
     (def ~name '~name)
     '(:constant ~name ~value)))
(defn write-constant [wrtr depth [name value]]
  (list (indent depth) name " = " value ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mesh
(defn fa! [x]
  `(:fa ~x))
(defn write-fa [depth x]
  (list (indent depth) "$fa = " x ";\n"))

(defn fn! [x]
  `(:fn ~x))
(defn write-fn [depth x]
  (list (indent depth) "$fn = " x ";\n"))

(defn fs! [x]
  `(:fs ~x))
(defn write-fs [depth x]
  (list (indent depth) "$fs = " x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defn cylinder [rs h]
  (match [rs]
    [[r1 r2]] `(:cylinder {:h ~h :r1 ~r1 :r2 ~r2})
    [r] `(:cylinder {:h ~h :r ~r} ~r)
    ))
(defn write-cylinder [depth [{h :h r :r r1 :r1 r2 :r2}]]
  (if (nil? r)
    (list (indent depth) "cylinder (h=" h ", r1=" r1 ", r2=" r2 ", center=true);\n")
    (list (indent depth) "cylinder (h=" h ", r=" r ", center=true);\n")))

(defn sphere [r]
  `(:sphere {:r ~r}))
(defn write-sphere [depth [{r :r}]]
  (list (indent depth) "sphere (r=" r ", center=true);\n"))

(defn cube [x y z]
  `(:cube {:x ~x :y ~y :z ~z}))
(defn write-cube [depth [{x :x y :y z :z}]]
  (list (indent depth) "cube(x=" x ", y=" y ", z=" z ", center=true);\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))
(defn write-translate [depth [[x y z] & block]]
  (list
   (list (indent depth) "translate ([" x "," y "," z "]) {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn rotate [a [x y z] & block]
  `(:rotate [~a [~x ~y ~z]] ~@block))
(defn write-rotate [depth [[a [x y z]] & block]]
  (list
   (list (indent depth) "rotate (a=" a ", v=[" x "," y "," z "]) {\n")
   ;;(apply str (map #(write-expr (+ depth 1) %1) block))
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))
(defn write-scale [depth [[x y z] & block]]
  (list
   (list (indent depth) "scale ([" x "," y "," z "]) {\n")
   ;;(apply str (map #(write-expr (+ depth 1) %1) block))
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn mirror [[x y z] & block]
  `(:mirror [~x ~y ~z] ~@block))
(defn write-mirror [depth [[x y z] & block]]
  (list
   (list (indent depth) "mirror ([" x "," y "," z "]) {\n")
   ;;(apply str (map #(write-expr (+ depth 1) %1) block))
   (write-block depth block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators
(defn union [ & block]
  `(:union  ~@block))
(defn write-union [depth [ & block]]
  (list
   (list (indent depth) "union () {\n")
   ;;(apply str (flatten (map #(write-expr (+ depth 1) %1) block)))
   (write-block depth block)
   (list (indent depth) "}\n")))

(defn intersection [ & block]
  `(:intersection  ~@block))
(defn write-intersection [depth [ & block]]
  (list
   (list (indent depth) "intersection () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn hull [ & block]
  `(:hull  ~@block))
(defn write-hull [depth [ & block]]
  (list
   (list (indent depth) "hull () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defn difference [ & block]
  `(:difference  ~@block))
(defn write-difference [depth [ & block]]
  (list
   (list (indent depth) "difference () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules
(defmacro module [name args & block]
  `(do
     (defn ~name [~@args] (list ':call '~name [~@args]))
     ~@(map (fn [x] `(def ~x '~x)) args)
     (let [r# (list :module '~name '~args ~@block)]
       ~@(map (fn [x] `(ns-unmap *ns* '~x)) args)
       r#)
     ))

(defn write-module [depth [name args & block]]
  (list
   (list (indent depth) "\nmodule " name "(" (join " " args) ") {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n\n")))
(defn write-call [depth [name [& args]]]
  (list (indent depth) name "(" (join " " args) ");\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def form-writer
  {
   :constant write-constant

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

   :module write-module
   :call write-call 
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
