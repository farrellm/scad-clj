;; Base JBullet code from Nurullah Akkaya, see references/ball/core.clj

(ns scad-clj.physics
  (:use [incanter.core :only [view]])
  (:use [incanter.processing :only [sketch size framerate smooth]])
  (:import
   (javax.vecmath Vector3f Quat4f)
   (com.bulletphysics.dynamics DiscreteDynamicsWorld
                               RigidBodyConstructionInfo
                               RigidBody)
   (com.bulletphysics.collision.dispatch DefaultCollisionConfiguration
                                         CollisionDispatcher)
   (com.bulletphysics.dynamics.constraintsolver
    SequentialImpulseConstraintSolver)
   (com.bulletphysics.collision.broadphase AxisSweep3)
   (com.bulletphysics.linearmath Transform DefaultMotionState)
   (com.bulletphysics.collision.shapes StaticPlaneShape
                                       BoxShape
                                       SphereShape)))

(defn world []
  (let [maxProxies 1024
        worldAabbMin (Vector3f. -10000 -10000 -10000)
        worldAabbMax (Vector3f.  10000  10000  10000)
        cc (DefaultCollisionConfiguration.)]
    (doto (DiscreteDynamicsWorld.
           (CollisionDispatcher. cc)
           (AxisSweep3. worldAabbMin worldAabbMax maxProxies)
           (SequentialImpulseConstraintSolver.)
           cc)
      (.setGravity (Vector3f. 0 -60 0)))))

(defn surface [world]
  (let [shape (StaticPlaneShape. (Vector3f. 0 1 0)  0)
        motion-state (DefaultMotionState.
                       (doto (Transform.)
                         (-> .origin (.set (Vector3f. 0 0 0)))
                         (.setRotation (Quat4f. 0 0 0 1))))
        construction-info (RigidBodyConstructionInfo.
                           0 motion-state shape (Vector3f. 0 0 0))
        rigid-body (RigidBody. construction-info)]
    (.addRigidBody world rigid-body)
    rigid-body))

(defn box [world [x y]]
  (let [fall-mass 1
        fall-inertia (Vector3f. 10 0 0)
        shape (doto (BoxShape. (Vector3f. 3 30 15))
                (.calculateLocalInertia fall-mass fall-inertia))
        motion-state (DefaultMotionState.
                       (doto (Transform.)
                         (-> .origin (.set (Vector3f. x y 0)))
                         (.setRotation (Quat4f. 0 0 0 1))))
        construction-info (RigidBodyConstructionInfo.
                           fall-mass motion-state shape fall-inertia)
        rigid-body (RigidBody. construction-info)]
    (.addRigidBody world rigid-body)
    rigid-body))

(defn sphere [world [x y]]
  (let [fall-mass 1
        fall-inertia (Vector3f. 10 0 0)
        shape (doto (SphereShape. 10.0)
                (.calculateLocalInertia fall-mass fall-inertia))
        motion-state (DefaultMotionState.
                       (doto (Transform.)
                         (-> .origin (.set (Vector3f. x y 0)))
                         (.setRotation (Quat4f. 0 0 0 1))))
        construction-info (RigidBodyConstructionInfo.
                           fall-mass motion-state shape fall-inertia)
        rigid-body (RigidBody. construction-info)]
    (.addRigidBody world rigid-body)
    rigid-body))

(defn coords [body]
  (let [transform (Transform.)]
    (-> (.getMotionState body) (.getWorldTransform transform))
    [(-> transform .origin .x)
     (-> transform .origin .y)
     (-> transform .origin .z)
     (-> transform .basis .m00)
     (-> transform .basis .m01)
     (-> transform .basis .m02)
     (-> transform .basis .m10)
     (-> transform .basis .m11)
     (-> transform .basis .m12)
     (-> transform .basis .m20)
     (-> transform .basis .m21)
     (-> transform .basis .m22)]))

(defn draw-body [applet body]
  (let [[x y z m00 m01 m02 m10 m11 m12 m20 m21 m22] (coords body)]
    ;(println (str x " " y " " z))
    (doto applet
      (.pushMatrix)
      (.translate (/ (.width applet) 2) (* (.height applet) 0.8) 0)
      (.translate x (- y) z)
      (.applyMatrix m00 m01 m02 0
                    m10 m11 m12 0
                    m20 m21 m22 0
                    0 0 0 1))
    (if (instance? BoxShape (.getCollisionShape body))
      (.box applet 6 60 30)
      (.sphere applet 10))
    (.popMatrix applet)))

(defn draw-floor [applet]
  (doto applet
    (.pushMatrix)
    (.translate (/ (.width applet) 2) (* (.height applet) 0.8) 0)
    )
   (.box applet 1000 0.001 1000)
  (.popMatrix applet))

(defn make-sphere [world]
  (let [fall-shape (SphereShape. 1)
        motion-state (DefaultMotionState.
                       (doto (Transform.)
                         (-> .origin (.set (Vector3f. 0 50 0)))
                         (.setRotation (Quat4f. 0 0 0 1))))
        mass 1
        fall-inertia (doto (Vector3f. 0 0 0)
                       #(.calculateLocalInertia fall-shape mass %1))
        body-info (RigidBodyConstructionInfo. mass motion-state fall-shape fall-inertia)
        body (RigidBody. body-info)]
    (.addRigidBody world body)
    body
    ))

(defn frame []
  (let [fps 24
        world (world)
        surface (surface world)
        sphere (make-sphere world)
        bodies (ref (map #(box world %) (for [x (range 200 500 60)
                                              y (range 300 1 -60.1)]
                                          [x y])))
        ]
    ;;(dosync (alter bodies conj sphere))
    (sketch
     (setup []
            (doto this
              (size 640 370 incanter.processing/P3D)
              (.noStroke)
              (framerate fps)
              smooth))
     (draw []
           (.stepSimulation world (/ 1 fps) 8)
           (doto this
             (.background 50)
             (.lights))
           ;; (doseq [body @bodies]
           ;;   (draw-body this body))
           (draw-floor this)
           (draw-body this sphere)
           )
     ;; (mousePressed [e]
     ;;               (let [cords [(.mouseX this) (.mouseY this)]
     ;;                     sphere (sphere world cords)]
     ;;                 (.setLinearVelocity sphere (Vector3f. -2000 0 0))
     ;;                 (dosync (alter bodies conj sphere))))
     )))

;; (use 'scad-clj.physics)
;; (use '[incanter.core :only [view]])
;; (use '[incanter.processing :only [sketch size framerate smooth]])
(view (frame) :size [640 400])

;; (if true
;;   (let [f (view (frame) :size [640 400])]
;;     (.setSize f
;;               (+ (.getWidth f) (- (.getWidth f)
;;                                    (.getWidth (.getRootPane f))))
;;               (+ (.getHeight f) (- (.getHeight f)
;;                                    (.getHeight (.getRootPane f))))
;;               ))
;;   ;; (doto (view (frame) :size [640 400])
;;   ;;   (.setSize 640 400))
;;   )