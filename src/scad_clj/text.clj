(ns scad-clj.text
  (:use [clojure.core.match :only (match)])
  (:import (org.opencv.core Core CvType Mat MatOfPoint2f Point Scalar Size))
  (:import (org.opencv.highgui Highgui))
  (:import (org.opencv.imgproc Imgproc))
  (:import (scad_clj.library LibraryLoader))
  (:use [clojure.pprint])
  )

(LibraryLoader/force)

(defn- p+ [p1 p2] (Point. (+ (.x p1) (.x p2)) (+ (.y p1) (.y p2))))

;; Utils/bitmapToMat

(defn text->polygons [txt & {:keys [face size]}]
  (let [
        font-face (if (= face :script) Core/FONT_HERSHEY_SCRIPT_SIMPLEX
                    Core/FONT_HERSHEY_PLAIN)
        font-scale size
        thickness size
        baseline 1

        text-size (Core/getTextSize txt font-face font-scale
                                    thickness (int-array `(~baseline)))

        text-size-2 (Size. (.width text-size) (* 2 (.height text-size)))

        img (Mat. text-size-2 CvType/CV_8UC3 (Scalar/all 0.0))
        ret (Mat. text-size-2 CvType/CV_8UC3)
        tmp (Mat. text-size-2 CvType/CV_8UC3)
        
        text-org (Point. (/ (- (.cols img) (.width text-size)) 2)
                         (/ (+ (.rows img) (.height text-size)) 2))

        contours (java.util.LinkedList.)
        hierarchy (Mat.)]

    (Core/putText img txt text-org font-face font-scale
                  (Scalar/all 255) thickness)
    (Imgproc/cvtColor img tmp Imgproc/COLOR_BGR2GRAY)

    (Imgproc/findContours tmp contours hierarchy
                          Imgproc/RETR_CCOMP
                          Imgproc/CHAIN_APPROX_SIMPLE)

    ;; (Highgui/imwrite "out.png" img)

    (declare make-node)
    (defn make-tree [i]
      (let [node (make-node i)
            next (int (aget (.get hierarchy 0 i) 0))]
        (if (= next -1) (list node)
            (cons node (make-tree next))))
      )
    (defn make-node [i]
      (let [child (int (aget (.get hierarchy 0 i) 2))]
        (if (= child -1) i
            (cons i (make-tree child))))
      )

    (defn mat->points [mat]
      (map (fn [p] [(.x p) (.y p)])
           (.toList mat)))

    (let [approx 
          (map #(let [c (MatOfPoint2f.)
                      a (MatOfPoint2f.)]
                  (.fromList c (.toList %1))
                  (Imgproc/approxPolyDP c a 1 true)
                  a)
               contours)
          tree (make-tree 0)]
      (map (fn [l]
             (if (coll? l)
               (map #(mat->points (.get approx %1)) l)
               (list (mat->points (.get approx l)))
               ))
           tree)
      )))
