(ns scad-clj.text
  (:import (java.awt Font RenderingHints)
           (java.awt.font FontRenderContext)
           (java.awt.geom PathIterator)))

(def segment-type
  {PathIterator/SEG_CLOSE :close
   PathIterator/SEG_CUBICTO :cubic-to
   PathIterator/SEG_LINETO :line-to
   PathIterator/SEG_MOVETO :move-to
   PathIterator/SEG_QUADTO :quad-to})

;; How many points are specified for each segment type
(def segment-length
  {PathIterator/SEG_CLOSE 0
   PathIterator/SEG_CUBICTO 3
   PathIterator/SEG_LINETO 1
   PathIterator/SEG_MOVETO 1
   PathIterator/SEG_QUADTO 2})

(defn- path-iterator->segments
  "Converts a PathIterator into a sequence of segments of the
  form [segment-type [& points]]."
  [^PathIterator path-iterator]
  (if (not (.isDone path-iterator))
    (let [coords (double-array (* 2 (apply max (vals segment-length))))
          segment-code (.currentSegment path-iterator coords)]
      (cons [(segment-type segment-code)
             (take (segment-length segment-code) (partition 2 coords))]
            (lazy-seq (path-iterator->segments (doto path-iterator (.next))))))))

(defn- quad->fn
  "Returns the parametric control equation f(t), 0 <= t <= 1 for the
  quadratic interpolation of 3 points."
  [cp p1 p2]
  (fn [t]
    (letfn [(interp [a b c] (+ (* (Math/pow (- 1 t) 2) a)
                               (* 2 t (- 1 t) b)
                               (* (Math/pow t 2) c)))]
      [(apply interp (map first [cp p1 p2]))
       (apply interp (map second [cp p1 p2]))])))

(defn- cubic->fn
  "Returns the parametric control equation f(t), 0 <= t <= 1 for the
  cubic interpolation of 4 points."
  [cp p1 p2 p3]
  (fn [t]
    (letfn [(interp [a b c d]
              (+ (* (Math/pow (- 1 t) 3) a)
                 (* 3 t (Math/pow (- 1 t) 2) b)
                 (* 3 (Math/pow t 2) (- 1 t) c)
                 (* (Math/pow t 3) d)))]
      [(apply interp (map first [cp p1 p2 p3]))
       (apply interp (map second [cp p1 p2 p3]))])))

(defn- segments->lines
  "Takes a sequence of segments of the form [segment-type [& points]]
  and transforms each segment into a sequence of interpolated points."
  [segments]
  (reductions (fn [prev-line-points [segment-type control-points]]
                #_(println segment-type)
                (condp = segment-type
                  :move-to control-points
                  :line-to control-points
                  :quad-to (map (apply quad->fn
                                       (last prev-line-points)
                                       control-points)
                                (range 1/10 11/10 1/10))
                  :cubic-to (map (apply cubic->fn
                                        (last prev-line-points)
                                        control-points)
                                 (range 1/10 11/10 1/10))))
              (last (rest (first segments)))
              (rest segments)))

(defn- path2d [points]
  (let [path (doto (java.awt.geom.Path2D$Double.)
               (.moveTo (-> points first first)
                        (-> points first second)))]
    (doseq [point (rest points)]
      (.lineTo path (first point) (second point)))
    path))

(defn- split-even-odd-intersecting [paths]
  (let [poly-cache (zipmap paths (map path2d paths))
        poly-intersects-path? (fn [poly path]
                                (some #(.contains poly
                                                  (first %) (second %))
                                      path))
        count-intersections (fn [path] (count
                                        (filter #(or (= path %)
                                                     (poly-intersects-path? (poly-cache %) path))
                                                paths)))]
    (group-by count-intersections paths)))

(defn text-parts [font size text]
  (let [frc (FontRenderContext. nil
                                RenderingHints/VALUE_TEXT_ANTIALIAS_DEFAULT
                                RenderingHints/VALUE_FRACTIONALMETRICS_DEFAULT)
        glyph-vector (.createGlyphVector (Font. font Font/PLAIN size) frc text)
        path-iters (map #(-> glyph-vector
                             (.getGlyphOutline %)
                             (.getPathIterator nil))
                        (range (.getNumGlyphs glyph-vector)))
        intersection-count-maps
          (map (fn [path-iter] (->> (path-iterator->segments path-iter)
                                    (partition-by #(= (first %) :close))
                                    (take-nth 2)
                                    (map segments->lines)
                                    (map flatten)
                                    (map (partial partition 2))
                                    split-even-odd-intersecting))
               path-iters)]
    (->> (apply merge-with concat intersection-count-maps)
         (sort-by first)
         (map second))))
