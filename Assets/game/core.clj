(ns game.core
  (:use [arcadia.core]
        [arcadia.linear]
        [hard.core]
        [hard.input])
  ;; (:import
  ;;  [Scripts.Knight]
  ;;  [Scripts.Bishop]
  ;;  )
  )

(defn spawn [board key]
  (child+ board (->go (Bishop.))))


(defn start-game [o]
  (clear-cloned!)
  (clone! :camera)
  (clone! :light)
  (let [board (clone! :board)
        king (clone! :wking)]
    (child+ board king)
    ;; (hook+ ship :update :move-ship #'game.core/move-ship)
    ))

(start-game nil)

(rotate! (object-named "wking") (v3 0 90 0))

(.isWhite (object-named "wking"))

(instance? Chessman (object-named "wking"))

(.. isWhite (object-named "wking"))
