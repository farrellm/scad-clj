(ns scad-clj.model
  (:use [clojure.core.match :only (match)])
  (:use [clojure.pprint])
  (:use [scad-clj.text :only (text-parts)])  
  )

(def tau (* 2 Math/PI))
(def pi Math/PI)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2d primitives

(defn square [x y]
  `(:square {:x ~x :y ~y}))

(defn circle [r]
  `(:circle {:r ~r}))

(defn polygon
  ([points]
     `(:polygon {:points ~points}))
  ([points paths & {:keys [convexity]}]
     `(:polygon {:points ~points :paths ~paths :convexity ~convexity}))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projection/extrusion

(defn projection [cut & block]
  `(:projection {:cut cut} ~@block))

(defn cut [& block]
  `(:projection {:cut true} ~@block))

(defn project [& block]
  `(:projection {:cut false} ~@block))

(defn extrude-linear [{:keys [height twist convexity]} & block]
  `(:extrude-linear {:height ~height :twist ~twist :convexity ~convexity} ~@block))

(defn extrude-rotate
  ([ block ] `(:extrude-rotate {} ~block))
  ([{:keys [convexity]} block] `(:extrude-rotate {:convexity ~convexity} ~block))
  )

(let [{:keys [a]} {:a 3}] a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text

(defn text [font size text]
  (let [even-odd-paths (text-parts font size text)]
    (:shape
     (reduce (fn [{:keys [union? shape]} paths]
               (if union?
                 {:union? false
                  :shape (apply union shape (map polygon paths))}
                 {:union? true
                  :shape (apply difference shape (map polygon paths))}))
             {:union? true}
             even-odd-paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defn render [ & block]
  `(:render ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended


(defn extrude-curve [{:keys [height radius angle n]} block]
  (let [lim (Math/floor (/ n 2))
        phi (/ (/ angle (- n 1)) 2)]
    (apply union
           (map (fn [x]
                  (let [theta (* 0.5 angle (/ x lim) )
                        r radius
                        dx (* r (- (Math/sin theta)
                                   (* theta (Math/cos theta))))
                        dz (* r (+ (Math/cos theta)
                                   (* theta (Math/sin theta)) (- 1)))]
                    (translate [(+ dx (* 0 (Math/sin theta) (/ height 2)))
                                0
                                (+ dz (* 0 (Math/cos theta) (/ height 2)))]
                      (rotate theta [0 1 0]
                        (intersection
                         (translate [(* r theta) 0 0]
                           (cube (* 2 (+  r height) (Math/sin phi))
                                 1000 (* 2 height)))
                         (extrude-linear {:height height}
                           block))))))
                (range (- lim) (+ lim 1))))))

