(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  (:use [clojure.core.match :only (match)])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forward declarations
(declare write-expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
(defn indent [depth]
  (apply str (repeat depth "  ")))

(defmacro constant [name value]
  `(do
     (def ~name '~name)
     '(:constant ~name ~value)))
(defn write-constant [wrtr depth [name value]]
  (.write wrtr (str (indent depth) name " = " value ";\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math
;; (defn s+ [& args]
;;   `(:math-infix + [~@args]))
;; (defn s- [& args]
;;   `(:math-infix - [~@args]))
;; (defn s* [& args]
;;   `(:math-infix * [~@args]))
;; (defn sdiv [& args]
;;   `(:math-infix / [~@args]))
;; (defn eval-math-infix [op [& args]]
;;   (join args op))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
(defn cylinder [rs h]
  (match [rs]
    [[r1 r2]] `(:cylinder {:h ~h :r1 ~r1 :r2 ~r2})
    [r] `(:cylinder {:h ~h :r ~r} ~r)
    ))
(defn write-cylinder [wrtr depth [{h :h r :r r1 :r1 r2 :r2}]]
  (if (nil? r)
    (.write wrtr (str (indent depth) "cylinder (h=" h ", r1=" r1 ", r2=" r2 ", center=true);\n"))
    (.write wrtr (str (indent depth) "cylinder (h=" h ", r=" r ", center=true);\n"))))

(defn sphere [r]
  `(:sphere {:r ~r}))
(defn write-sphere [wrtr depth [{r :r}]]
  (.write wrtr (str (indent depth) "sphere (r=" r ", center=true);\n")))

(defn cube [x y z]
  `(:cube {:x ~x :y ~y :z ~z}))
(defn write-cube [wrtr depth [{x :x y :y z :z}]]
  (.write wrtr (str (indent depth) "cube(x=" x ", y=" y ", z=" z ", center=true);\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))
(defn write-translate [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "translate ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn rotate [[a [x y z]] & block]
  `(:rotate [~a [~x ~y ~z]] ~@block))
(defn write-rotate [wrtr depth [[a [x y z]] & block]]
  (.write wrtr (str (indent depth) "rotate (a=" a " v=[" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))
(defn write-scale [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "scale ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn mirror [[x y z] & block]
  `(:mirror [~x ~y ~z] ~@block))
(defn write-mirror [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "mirror ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators
(defn union [ & block]
  `(:union  ~@block))
(defn write-union [wrtr depth [ & block]]
  (.write wrtr (str (indent depth) "union () {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn intersection [ & block]
  `(:intersection  ~@block))
(defn write-intersection [wrtr depth [ & block]]
  (.write wrtr (str (indent depth) "intersection () {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn hull [ & block]
  `(:hull  ~@block))
(defn write-hull [wrtr depth [ & block]]
  (.write wrtr (str (indent depth) "hull () {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn difference [ & block]
  `(:difference  ~@block))
(defn write-difference [wrtr depth [ & block]]
  (.write wrtr (str (indent depth) "difference () {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

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

(defn write-module [wrtr depth [name args & block]]
  (.write wrtr (str (indent depth) "\nmodule " name "(" (join " " args) ") {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n\n"))
  )
(defn write-call [wrtr depth [name [& args]]]
  (.write wrtr (str (indent depth) name "(" (join " " args) ");\n"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def form-writer
  {:constant write-constant

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
(defn write-expr [wrtr depth [form & args]]
  (if (keyword? form)
    (apply (get form-writer form) (list wrtr depth args))
    (write-list wrtr depth (cons form args))
    ))

(defn write-list [wrtr depth list]
  (doall (map #(write-expr wrtr depth %1) list)))

(defn write-scad [path & block]
  (with-open [wrtr (writer path)]
    (write-list wrtr 0 block)))

