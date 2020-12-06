(ns game.core
  (:use [arcadia.core]
        [arcadia.linear]
        [hard.core]
        [hard.input])
  (:require [game.data :refer [get-pieces-names get-piece-pos get-piece-color get-piece-moves is-valid-move?
                               update-board! board-select! board-unselect! is-piece-selected? any-piece-selected?
                               does-piece-capture? get-piece-at-pos is-pieces-turn? update-board-turn!]] :reload)
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
      [x y])))

(defn highlight-piece! [go]
  ;; if selected, change material
  (let [texture (.mainTexture (.material (cmpt go MeshRenderer)))]
    (set! (.mainTexture (resource "highlight")) texture)
    (set! (.material (cmpt go MeshRenderer)) (resource "highlight"))))

(defn hide-highlight! [go]
  (set! (.material (cmpt go MeshRenderer)) (state go :material)))

(defn spawn-cell! [go [x y]]
  (let [highcell (clone! "highcell")]
    (position! highcell (v3 (+ x offset) 0.0001 (+ y offset)))
    ;; (child+ go highcell)
    ))

(defn highlight-cells! [go]
  (let [legal-moves (game.data/get-piece-moves (.name go))]
    (dorun (map (partial spawn-cell! go) legal-moves))))

(defn destroy-highcells! []
  (destroy! (every "highcell")))
#_(destroy-highcells!)

(defn select-piece! [go _]
  (when (and (mouse-down?) (is-pieces-turn? (.name go)))
    (let [[mouse-x mouse-y] (mouse-position)
          go-x              (int (.x (.position (.transform go))))
          go-y              (int (.z (.position (.transform go))))]
      (when (and (= mouse-x go-x) (= mouse-y go-y) (not (any-piece-selected?)))
        (highlight-cells! go)
        (highlight-piece! go)
        (board-select! (.name go))
        ))))

(defn unselect-piece! [go _]
  (when (and (mouse-down? 1) (is-piece-selected? (.name go)))
    (hide-highlight! go)
    (destroy-highcells!)
    (board-unselect! (.name go))
    ))

;; (board-unselect! "wbishop")
;; (is-piece-selected? "wbishop")

;;; move
(defn move-go! [go x y]
  (position! go (v3 (+ x offset) 0 (+ y offset))))

(defn go-position [go]
  [(int (.x (.position (.transform go))))
   (int (.z (.position (.transform go))))])

(defn move-piece! [go _]
  (when (and (mouse-down?) (is-piece-selected? (.name go)))
    (let [[mouse-x mouse-y] (mouse-position)
          name              (.name go)]
      (when (game.data/is-valid-move? name [mouse-x mouse-y])
        (when (game.data/does-piece-capture? name [mouse-x mouse-y])
          (destroy! (object-named (get-piece-at-pos [mouse-x mouse-y]))))
        (update-board! name [mouse-x mouse-y])
        (move-go! go mouse-x mouse-y)
        (hide-highlight! go)
        (destroy-highcells!)
        (board-unselect! name)
        (update-board-turn!)
        ))))

#_(destroy! (object-named (get-piece-at-pos (mouse-position))))

#_(game.data/is-legal-move? "wbishop" (mouse-position))

#_(hook (the "wbishop") :update :move)

;;; spawn
(defn spawn-piece! [board piece-name]
  (let [[x y] (get-piece-pos piece-name)
        go    (clone! piece-name)]
    (child+ board go)
    (move-go! go x y)
                                        ; TODO if white, rotate y -90
    (clear-hooks go :update)
    (state+ go :material (.material (cmpt go MeshRenderer)))
    (hook+ go :update :select #'game.core/select-piece!)
    (hook+ go :update :unselect #'game.core/unselect-piece!)
    (hook+ go :update :move #'game.core/move-piece!)
    ))


(defn start-game [o]
  (clear-cloned!)
  (clone! :camera)
  (clone! :light)
  (let [board (clone! :board)]
    (for [piece-name (get-pieces-names)]
      (spawn-piece! board piece-name))
    ))

(start-game nil)

;; (state (the "wking"))

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
