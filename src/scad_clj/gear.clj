(ns scad-clj.gear
  (:require [scad-clj.model :refer :all :exclude [import use]]
            [scad-clj.scad :refer :all]
            [clojure.core.matrix :refer [matrix mmul]])
  (:import [java.lang.Math]))

(def ^:dynamic *n-involute* 5)

(defn sq [x]
  (* x x))

(defn rad->deg [a]
  (* 360 (/ a tau)))

(defn rot-z [t]
  (matrix [[(Math/cos t)     (Math/sin t) 0.]
           [(- (Math/sin t)) (Math/cos t) 0.]
           [0.               0.           1.]]))

(defn involute-curve [i]
  (fn [t]
    (let [x (* i t)
          h (Math/sqrt (+ (sq x) (sq i)))]
      (mmul (rot-z (- t))
            (matrix [x i 0])))))

(defn samples [min max n]
  (let [diff (- max min)]
    (map #(+ min (* (/ % (dec n)) diff))
         (range n))))

(defn base-radius [{:keys [pitch-radius pressure-angle]}]
  (* pitch-radius (Math/cos pressure-angle)))

(defn dedendum [& {:keys [pitch-radius diametrical-pitch pressure-angle] :as args}]
  (- pitch-radius (base-radius args)))

(defn profile [{:keys [pitch-radius pressure-angle addendum] :as args}]
  (let [addendum-radius (+ pitch-radius addendum)
        base-radius (base-radius args)
        
        theta-addendum (Math/sqrt (- (sq (/ addendum-radius base-radius)) 1))
        theta-pitch (Math/sqrt (- (sq (/ pitch-radius base-radius)) 1))
        
        curve (involute-curve base-radius)
        [p-x _ _] (curve theta-pitch)

        alpha-pitch (Math/asin (/ p-x pitch-radius))]
    (rotate alpha-pitch [0 0 1]
            (polygon (conj (map (fn [[x y z]] [x y])
                                (map curve
                                     (samples 0 theta-addendum *n-involute*)))
                           [0 0])))))

(defn tooth [{:keys [pitch-radius radial-pitch pressure-angle addendum] :as args}]
  (let [profile (profile args)
        addendum-radius (+ pitch-radius addendum)]
    (hull
     profile
     (rotate (/ tau 2 radial-pitch pitch-radius) [0 0 1]
             (mirror [1 0 0]
                     profile)))))

;; radial-pitch: teath per tau units of circumference
(defn gear [{:keys [pitch-radius radial-pitch pressure-angle
                    tooth-ratio addendum] :as args}]
  (let [tooth (tooth args)]
    (rotate (/ tau 4) [0 0 -1]
            (union
             (circle (base-radius args))
             (apply union
                    (map (fn [i] (rotate (* i (/ tau radial-pitch pitch-radius)) [0 0 1] tooth))
                         (range (* radial-pitch pitch-radius))))))))

(defn angular-width [{:keys [pitch-radius radial-pitch] :as args}]
  (/ tau radial-pitch pitch-radius))

(defn tooth-position [{:keys [pitch-radius radial-pitch] :as args} rotation angle]
  (/ (rem (- angle rotation) (angular-width args))
     (angular-width args)))

(defn mate-to [args1 rot1 args2 rot2 theta]
  (let [pos1 (tooth-position args1 rot1 theta)
        pos2 (tooth-position args2 rot2 (+ theta (/ tau 2)))
        tgt (- 1 pos1)
        phi (* (- pos2 tgt) (angular-width args2))]
    ;; [pos1 pos2 tgt phi]
    phi
    ))

(defn planetary [args1 args2 n]
  (union
   (gear args1)
   (map
    (bound-fn [i]
      (let [theta (* i (/ tau n))]
        (rotate theta [0 0 1]
                (translate [(+ (:pitch-radius args1)
                               (:pitch-radius args2)) 0 0]
                           (rotate
                            (mate-to args1 0 args2 theta theta) [0 0 1]
                            (gear args2))))))
    (range n))))

;; sample spur gear

(def args1 {:pitch-radius 10
            :radial-pitch 3.0
            :pressure-angle (* tau (/ 20 360))
            :addendum 0.3})
(def args2 (assoc args1
             :pitch-radius 5))


(tooth-position args2 0 (/ tau 2))

(comment
  (extrude-linear {:height 3}
                  (union
                   (gear args1)
                   (translate [15 0 0]
                              (rotate
                               (mate-to args1 0 args2 0 0) [0 0 1]
                               (gear args2))))))

(comment
  (extrude-linear {:height 3}
                  (planetary args1 args2 7)))
