(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  (:use [clojure.core.match :only (match)])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethod
(defmulti write-expr
  (fn [depth [form & args]]
    (if (keyword? form) form :list)))

(defmethod write-expr :default [depth [form & args]]
  `("//(" ~form ~args ")"))

(defmethod write-expr :list [depth [& args]]
  (map #(write-expr depth %1) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility
(defn indent [depth]
  (apply str (repeat depth "  ")))

(defn write-block [depth block]
  (flatten (map #(write-expr (+ depth 1) %1) block)))

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
  (let [fargs (list (if (nil? fa) '() (str "$fa=" fa ", "))
                    (if (nil? fn) '() (str "$fn=" fn ", "))
                    (if (nil? fs) '() (str "$fs=" fs ", ")))]
    (list (indent depth) "cylinder (" fargs "h=" h
          (if (nil? r) (list ", r1=" r1 ", r2=" r2) (list ", r=" r))
          ", center=true);\n")))

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs]}]]
  (let [fargs (list (if (nil? fa) '() (str "$fa=" fa ", "))
                    (if (nil? fn) '() (str "$fn=" fn ", "))
                    (if (nil? fs) '() (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere (" fargs "r=" r ", center=true);\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z]}]]
  (list (indent depth) "cube(x=" x ", y=" y ", z=" z ", center=true);\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators
(defmethod write-expr :translate [depth [form [x y z] & block]]
  (list
   (list (indent depth) "translate ([" x "," y "," z "]) {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotate [depth [form [a [x y z]] & block]]
  (list
   (list (indent depth) "rotate (a=" (/ (* a 180) Math/PI) ", v=[" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :scale [depth [form [x y z] & block]]
  (list
   (list (indent depth) "scale ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror [depth [form [x y z] & block]]
  (list
   (list (indent depth) "mirror ([" x "," y "," z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinators
(defmethod write-expr :union [depth [form & block]]
  (list
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection [depth [form & block]]
  (list
   (list (indent depth) "intersection () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :hull [depth [form & block]]
  (list
   (list (indent depth) "hull () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference [depth [form & block]]
  (list
   (list (indent depth) "difference () {\n")
   (map #(write-expr (+ depth 1) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

(defn write-scad [& block]
  (apply str (flatten (write-expr 0 block))))

(defn write-scad-to-file [path & block]
  (with-open [wrtr (writer path)]
    (.write wrtr (write-scad block))))
