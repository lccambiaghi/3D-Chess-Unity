(ns game.data
  ;; (:require [arcadia.core :refer [log]])
  )

(def size 8)

;; TODO multimethod to get legal moves? default is current legal-move, more conditions for pawn and king

;;; pieces

(defn trajs [n directions]
  (let [moves              (for [step      (range 1 n)
                                 direction directions]
                             {direction [(* step (first direction)) (* step (second direction))]})
        moves-by-direction (apply merge-with into moves)
        trajectories       (map (fn [[k v]] (partition 2 v)) moves-by-direction)]
    trajectories))

(defmulti trajectories (fn [piece] (:type piece)))

(defmethod trajectories "king"
  [piece]
  (let [n          2 ; TODO change steps to be 1 and 7 and put +1 in trajs
        directions [[0 1]             ;up
                    [1 1]             ;up right
                    [-1 1]            ;up left
                    [0 -1]            ;down
                    [1 -1]            ;down right
                    [-1 -1]           ;down left
                    [1 0]             ;right
                    [-1 0]]]
    (trajs n directions)))

(defmethod trajectories "bishop"
  [piece]
  (let [n-cells   8
        direction [[1 1] [1 -1] [-1 1] [-1 -1]]]
    (trajs n-cells direction)))

(defmethod trajectories "pawn"
  [piece]
  (let [steps      2
        directions [[0 (if (= (:color piece) "white") 1 -1)]        ; forward
                    [0 (if (= (:color piece) "white") 2 -2)]        ; double forward
                    ]]
    (trajs steps directions)))

(defmethod trajectories "knight"
  [piece]
  (let [steps      3
        directions [[1 2]             ;up right
                    [-1 2]            ; up left
                    [1 -2]            ; bottom right
                    [-1 -2]           ; bottom left
                    [2 1]             ; right up
                    [2 -1]            ; right down
                    [-2 1]            ; left up
                    [-2 -1]           ; left down
                    ]]
    (trajs steps directions)))

(defmethod trajectories "rook"
  [piece]
  (let [steps      8
        directions [[0 1]             ;up
                    [0 -1]            ;down
                    [1 0]             ;right
                    [-1 0]            ;left
                    ]]
    (trajs steps directions)))

(defmethod trajectories "queen"
  [piece]
  (let [steps      8
        directions [[1 2]             ;up right
                    [-1 2]            ; up left
                    [1 -2]            ; bottom right
                    [-1 -2]           ; bottom left
                    [2 1]             ; right up
                    [2 -1]            ; right down
                    [-2 1]            ; left up
                    [-2 -1]           ; left down
                    ]]
    (trajs steps directions)))

(defmulti captures (fn [piece] (if (= (:type piece) "pawn") "pawn" "king")))

(defmethod captures "pawn"
  [piece]
  (let [steps      2
        directions [[1 (if (= (:color piece) "white") 1 -1)]        ; diag right
                    [-1 (if (= (:color piece) "white") 1 -1)]        ; diag left
                    ]]
    (trajs steps directions)))

(defmethod captures :default
  [piece]
  (trajectories piece)
  )



;;; board
(def board
  (atom
   {:turn "white"
    ;; white pieces
    [4 1] {:name "wking" :color "white" :type "king"}
    ;; [3 0] (map->Queen {:name "wqueen" :color "white"})
    [5 0] {:name "wbishop" :color "white" :type "bishop"}
    [6 6] {:name "bpawn" :color "black" :type "pawn"}
    ;; [7 0] (map->Rook {:name "wrook" :color "white"})
    ;; [5 0] (map->Bishop {:name "wbishop" :color "white"})
    ;; [6 0] (map->Knight {:name "wknight" :color "white"})
    ;; black pieces
    [3 7] {:name "bking" :color "black" :type "king"}
    }))

(defn get-pieces-names
  ([] (map :name (vals (dissoc @board :turn))))
  ([color] (get-pieces-names color @board))
  ([color board] (map :name (vals (filter (fn [[k v]] (= (:color v) color)) board)))))

(defn get-piece-map
  ([name] (get-piece-map name @board))
  ([name board] (first (filter (fn [[k v]] (= (:name v) name)) board)))) ; TODO why do we need first?

(defn get-piece
  ([name] (get-piece name @board))
  ([name board] (val (get-piece-map name board))))

(defn get-piece-pos
  ([name] (get-piece-pos name @board))
  ([name board] (key (get-piece-map name board))))

(defn get-piece-color
  ([name] (get-piece-color name @board))
  ([name board] (:color (get-piece name board))))

#_(get-pieces-of-color "white")

(defn is-pieces-turn? [name]
  (= (get-piece-color name) (:turn @board)))

(defn is-piece-selected? [name]
  (let [piece (get-piece name)]
    (get piece :selected)))


;;; get-piece-moves

(defn abs [x]
  (if (> x 0) x (* -1 x)))

(defn legal-pawn-move? [name [to-x to-y] board]
  "Pawns can only move by 2 if it is in initial position"
  (let [[from-x from-y]      (get-piece-pos name board)
        color                (get-piece-color name board)
        advances-two?        (= (abs (- to-y from-y)) 2)
        is-pawn-initial-pos? (if (= color "white") (= from-y 1) (= from-y 6))]
    (if advances-two? is-pawn-initial-pos? true)))

#_(legal-pawn-move? "bpawn" [6 1])
;; (is-legal-move? "wbishop" [6 1])

(declare legal-king-move?)

(defn is-legal-move? [name [x y] board]
  (and (not (contains? board [x y]))
       (>= x 0) (< x  size)
       (>= y 0) (< y size)
       (if (re-find #"pawn" name) (legal-pawn-move? name [x y] board) true)
       (if (re-find #"king" name) (legal-king-move? name [x y] board) true)
       ))

(defn is-enemy-cell? [color cell]
  (let [cell-color (:color (get @board cell))]
    (and (some? cell-color) (not= color cell-color))))

;; TODO pawn en-passant
(defn does-piece-capture? [name cell]
  (let [piece-color (get-piece-color name)]
    (is-enemy-cell? piece-color cell)))

(defn cut-traj
  "Keep the first part of the trajectory until we hit a piece."
  ([name traj] (cut-traj name traj @board))
  ([name traj board] (take-while (fn [move] (is-legal-move? name move board)) traj)))

(defn cut-captures [name traj]
  "Returns first enemy captured in trajectory"
  (let [moves    (sort-by #(+ (abs (first %)) (abs (second %))) traj)
        captures (filter (fn [move] (does-piece-capture? name move)) moves)]
    (if (empty? captures) '() (first captures))))

(defn add-xy [[x y] [x-offset y-offset]]
  [(+ x x-offset) (+ y y-offset)])

(defn flt [s] (mapcat #(if (every? coll? %) (flt %) (list %)) s))

(defn get-piece-moves [name]
  (let [[pos piece]    (get-piece-map name)
        maybe-moves    (map (fn [traj] (map (partial add-xy pos) traj)) (trajectories piece))
        legal-moves    (map (partial cut-traj name) maybe-moves)
        maybe-captures (map (fn [traj] (map (partial add-xy pos) traj)) (captures piece))
        legal-captures (map (partial cut-captures name) maybe-captures)]
    (concat (flt legal-moves) (flt legal-captures))))

;; (get-piece-moves "wking")

;; user selection

(defn board-select! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (assoc piece :selected true))))

(defn board-unselect! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (dissoc piece :selected))))

;; TODO unify with function above??
(defn is-valid-move? [name cell]
  (let [legal-moves (get-piece-moves name)]
    (some #(= cell %) legal-moves)))

(defn get-piece-at-pos [pos]
  (:name (get @board pos)))

#_(is-valid-move? "wbishop" [5 0])

(defn any-piece-selected? []
  (reduce (fn [res [pos piece]] (or res (get piece :selected))) false @board))

;; TODO
(defn has-legal-moves? [name]
  true
  )

;; update board

(defn update-board! [name new-pos]
  (let [[previous-pos piece] (get-piece-map name)
        new-color (if (= (:turn @board) "white") "black" "white")]
    (swap! board dissoc previous-pos)
    (swap! board assoc new-pos piece)
    (swap! board assoc :turn new-color)))

(defn update-board [name new-pos]
  (let [[previous-pos piece] (get-piece-map name)
        new-color (if (= (:turn @board) "white") "black" "white")]
    (-> (dissoc @board previous-pos)
        (assoc new-pos piece)
        ;; (assoc :turn new-color)
        )))

#_(update-board-turn!)

;; check, checkmate

(defn threathened-cells [board name]
  (let [[pos piece]    (get-piece-map name board)
        maybe-captures (map (fn [traj] (map (partial add-xy pos) traj)) (captures piece))
        legal-captures (map (fn [traj] (cut-traj name traj board)) maybe-captures)]
    legal-captures))

;; (threathened-cells @board "wbishop")

(defn is-move-check? [name move]
  (let [new-board    (update-board name move)
        king-pos     (get-piece-pos (str (first (:turn new-board)) "king"))
        enemy-color  (if (= (:turn new-board) "white") "black" "white")
        pieces       (get-pieces-names enemy-color new-board)
        king-threats (map (partial threathened-cells new-board) pieces)
        ]
    (some #(= king-pos %) (flt king-threats))
    ))

;; (is-move-check? "wking" [7 0])

(defn legal-king-move? [name new-king-pos board]
  (let [color (get-piece-color name)]
    (if (= color (:turn board))
      (not (is-move-check? name new-king-pos))
      true)))

;; (legal-king-move? "wking" [6 0] @board)

(defn is-checkmate? []
  (let [color       (:turn @board)
        name (str (first color) "king")
        king-moves  (get-piece-moves name)
        legal-moves (filter (fn [move] (legal-king-move? name move board)) king-moves)]
    (= 0 (count legal-moves))))
