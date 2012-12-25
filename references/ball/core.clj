;;
;; ----------------------------------------------------------------------------
;; "THE BEER-WARE LICENSE" (Revision 42):
;; <nurullah@nakkaya.com> wrote this file. As long as you retain this notice you
;; can do whatever you want with this stuff. If we meet some day, and you think
;; this stuff is worth it, you can buy me a beer in return Nurullah Akkaya
;; ----------------------------------------------------------------------------
;;

;; Available at:
;; http://nakkaya.com/2010/10/24/more-physics-with-clojure-jbullet-and-processing/

(ns ball.core
  (:use [incanter.core :only [view]])
  (:use [incanter.processing :exclude [box sphere]])
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
      (.setGravity (Vector3f. 0 60 0)))))

(defn surface [world]
  (let [shape (StaticPlaneShape. (Vector3f. 0 -1 0)  1)
        motion-state (DefaultMotionState.
                       (doto (Transform.)
                         (-> .origin (.set (Vector3f. 0 330 0)))
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
    (doto applet
      (.pushMatrix)
      (.translate x y z)
      (.applyMatrix m00 m01 m02 0
                    m10 m11 m12 0
                    m20 m21 m22 0
                    0 0 0 1))
    (if (instance? BoxShape (.getCollisionShape body))
      (.box applet 6 60 30)
      (.sphere applet 10))
    (.popMatrix applet)))

(defn frame []
  (let [fps 24
        world (world)
        surface (surface world)
        bodies (ref (map #(box world %) (for [x (range 200 500 60)
                                              y (range 300 1 -60.1)]
                                          [x y])))]
    (sketch
     (setup []
            (doto this
              (size 640 400 processing.core.PConstants/P3D)
              (.noStroke)
              (framerate fps)
              smooth))
     (draw []
           (.stepSimulation world (/ 1 fps) 8)
           (doto this
             (.background 50)
             (.lights))
           (doseq [body @bodies]
             (draw-body this body)))
     (mousePressed [e]
                   (let [cords [(.mouseX this) (.mouseY this)]
                         sphere (sphere world cords)]
                     (.setLinearVelocity sphere (Vector3f. -2000 0 0))
                     (dosync (alter bodies conj sphere)))))))

;; (view (frame) :size [640 400])