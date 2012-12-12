(ns scad-clj.scad
  (:use [clojure.java.io :only [writer]])
  (:use [clojure.string :only [join]])
  )

(declare write-expr)

(defn indent [depth]
  (apply str (repeat depth "  ")))

(defn constant [name value]
  `(:constant ~name ~value))
(defn write-constant [wrtr depth [name value]]
  (.write wrtr (str (indent depth) name " = " value ";\n")))

(defn cylinder [& {:keys [h r r1 r2]}]
  {:pre [(or (and (not (nil? r)) (nil? r1) (nil? r2))
             (and (nil? r) (not (nil? r1)) (not (nil? r2))))]}
  (if (nil? r)
    `(:cylinder {:h ~h :r1 ~r1 :r2 ~r2})
    `(:cylinder {:h ~h :r ~r})))
(defn write-cylinder [wrtr depth [{h :h r :r r1 :r1 r2 :r2}]]
  (if (nil? r)
    (.write wrtr (str (indent depth) "cylinder (h=" h ", r1=" r1 ", r2=" r2 ", center=true);\n"))
    (.write wrtr (str (indent depth) "cylinder (h=" h ", r=" r ", center=true);\n"))))

(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))
(defn write-translate [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "translate ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn rotate [[x y z] & block]
  `(:rotate [~x ~y ~z] ~@block))
(defn write-rotate [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "rotate ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))
(defn write-scale [wrtr depth [[x y z] & block]]
  (.write wrtr (str (indent depth) "scale ([" x "," y "," z "]) {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n")))

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

(defmacro module [name args & block]
  `(list :module '~name '~args ~@block))

(defn write-module [wrtr depth [name args & block]]
  (.write wrtr (str (indent depth) "module " name "(" (join " " args) ") {\n"))
  (doall (map #(write-expr wrtr (+ depth 1) %1) block))
  (.write wrtr (str (indent depth) "}\n"))
  )

(def form-writer
  {:constant write-constant

   :cylinder write-cylinder

   :translate write-translate
   :rotate write-rotate
   :scale write-scale

   :union write-union
   :intersection write-intersection
   :hull write-hull
   :difference write-difference

   :module write-module
   })

(defn write-expr [wrtr depth [form & args]]
  (apply (get form-writer form) (list wrtr depth args)))

(defn write-scad [path & block]
  (with-open [wrtr (writer path)]
    (doall (map #(write-expr wrtr 0 %1) block))))

