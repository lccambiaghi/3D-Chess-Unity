(ns game.core
  (:use [arcadia.core]
        [arcadia.linear]
        [hard.core]
        [hard.input])
  (:import
   Physics
   MeshRenderer
   [UnityEngine Vector3]))

(def offset 0.5)

;;; select

(defn mouse-position []
  (when-first [mouse-hit (ray-hits (mouse-ray))]
    (let [x (int (.. mouse-hit point x))
          y (int (.. mouse-hit point z))]
      #_(log "pointing at " x " and " y)
      [x y])))

(defn highlight! [go]
  ;; if selected, change material
  (let [texture (.mainTexture (.material (cmpt go MeshRenderer)))
        previous (.material (cmpt go MeshRenderer))]
    ;; (set! (.mainTexture (resource "highlight")) texture)
    (state+ go :material previous)
    (set! (.material (cmpt go MeshRenderer)) (resource "highlight"))))

(defn hide-highlight! [go]
  (set! (.material (cmpt go MeshRenderer)) (state go :material)))


(defn select [go _]
  (when (mouse-down?)
    (let [[mouse-x mouse-y] (mouse-position)
          go-x (int (.x (.position (.transform go))))
          go-y (int (.z (.position (.transform go))))]
      (when (and (= mouse-x go-x) (= mouse-y go-y))
        (state+ go :selected true)
        (highlight! go)))))

;;; move

(defn legal-move? [go mouse-x mouse-y]
  (let [[go-x go-y] (piece-position go)]
    (or (not= mouse-x go-x) (not= mouse-y go-y))))

(defn piece-position [go]
  [(int (.x (.position (.transform go))))
   (int (.z (.position (.transform go))))])

(defn move [go _]
  (when (and (mouse-down?) (state go :selected))
    (let [[mouse-x mouse-y] (mouse-position)]
      (when (legal-move? go mouse-x mouse-y)
        (state- go :selected)
        (position! go (v3 (+ mouse-x offset) 0 (+ mouse-y offset)))
        (hide-highlight! go)))))

(defn start-game [o]
  (clear-cloned!)
  (clone! :camera)
  (clone! :light)
  (let [board (clone! :board)
        king  (clone! :wking)]
    (child+ board king)
    (position! king (v3 3.5 0 0.5))
    (clear-hooks king :update)
    (hook+ king :update :select #'game.core/select)
    (hook+ king :update :move #'game.core/move)
    ))

(start-game nil)

(comment

  (rotate! (object-named "wking") (v3 0 90 0))

  (instance? UnityEngine.GameObject (object-named "wking"))

  ;; (def heroRb2D (. g GetComponent Rigidbody2D))
  ;; (.AddForce heroRb2D (Vector2. -50 50)))

  (..  (first (ray-hits (mouse-ray))) point x )
  ;; => #<UnityEngine.Vector3 (3.7, 0.0, 0.6)>

  (Selection/activeObject)

  )
