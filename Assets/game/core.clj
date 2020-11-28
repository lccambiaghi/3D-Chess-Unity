(ns game.core
  (:use [arcadia.core]
        [arcadia.linear]
        [hard.core]
        [hard.input])
  (:require [game.data :refer [initial-pos get-pieces-names get-piece-pos]] )
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
  (let [texture (.mainTexture (.material (cmpt go MeshRenderer)))]
    (set! (.mainTexture (resource "highlight")) texture)
    (set! (.material (cmpt go MeshRenderer)) (resource "highlight"))))

(defn hide-highlight! [go]
  (set! (.material (cmpt go MeshRenderer)) (state go :material)))


(defn select [go _]
  (when (mouse-down?)
    (let [[mouse-x mouse-y] (mouse-position)
          go-x              (int (.x (.position (.transform go))))
          go-y              (int (.z (.position (.transform go))))]
      (when (and (= mouse-x go-x) (= mouse-y go-y))
        (state+ go :selected true)
        (highlight! go)))))

;;; move
(defn move-piece! [go x y]
  (position! go (v3 (+ x offset) 0 (+ y offset))))

(defn piece-position [go]
  [(int (.x (.position (.transform go))))
   (int (.z (.position (.transform go))))])

(defn legal-move? [go mouse-x mouse-y]
  (let [[go-x go-y] (piece-position go)]
    (or (not= mouse-x go-x) (not= mouse-y go-y))))

(defn move [go _]
  (when (and (mouse-down?) (state go :selected))
    (let [[mouse-x mouse-y] (mouse-position)]
      (when (legal-move? go mouse-x mouse-y)
        (state- go :selected)
        (hide-highlight! go)
        (position! go (v3 (+ mouse-x offset) 0 (+ mouse-y offset)))))))

;;; spawn
(defn spawn! [board piece-name]
  (let [[x y] (get-piece-pos piece-name)
        go    (clone! piece-name)]
    (child+ board go)
    (move-piece! go x y)
    (clear-hooks go :update)
    (state+ go :material (.material (cmpt go MeshRenderer)))
    (hook+ go :update :select #'game.core/select)
    (hook+ go :update :move #'game.core/move)))


(defn start-game [o]
  (clear-cloned!)
  (clone! :camera)
  (clone! :light)
  (let [board (clone! :board)]
    (for [piece-name (get-pieces-names)]
      (spawn! board piece-name))
    ))

(start-game nil)

(comment
  (hook  (object-named "wking") :update :move)
  (clear-hooks (object-named "wking") :update)
  (.name (object-named "wking"))

  (rotate! (object-named "wking") (v3 0 90 0))

  (position! (object-named "bking") (get initial-pos "bking"))

  (instance? UnityEngine.GameObject (object-named "wking"))

  ;; (def heroRb2D (. g GetComponent Rigidbody2D))
  ;; (.AddForce heroRb2D (Vector2. -50 50)))

  (..  (first (ray-hits (mouse-ray))) point x )
  ;; => #<UnityEngine.Vector3 (3.7, 0.0, 0.6)>
  ()

  (Selection/activeObject)

  )
