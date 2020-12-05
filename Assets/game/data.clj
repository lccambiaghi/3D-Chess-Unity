(ns game.data
  (:require [arcadia.core :refer [log]]))

(def size 8)


;; TODO multimethod to get legal moves? default is current legal-move, more conditions for pawn and king

;;; pieces
;; (defmulti trajectories (fn [piece] (:type piece)))
;; (defmethod trajectories :king
;;   [piece]
;;   (let [n          2 ; TODO change steps to be 1 and 7 and put +1 in trajs
;;         directions [[0 1]             ;up
;;                     [1 1]             ;up right
;;                     [-1 1]            ;up left
;;                     [0 -1]            ;down
;;                     [1 -1]            ;down right
;;                     [-1 -1]           ;down left
;;                     [1 0]             ;right
;;                     [-1 0]]]
;;     (trajs n directions)))

(defn trajs [n directions]
  (let [moves              (for [step      (range 1 n)
                                 direction directions]
                             {direction [(* step (first direction)) (* step (second direction))]})
        moves-by-direction (apply merge-with into moves)
        trajectories       (map (fn [[k v]] (partition 2 v)) moves-by-direction)]
    trajectories))

(defprotocol ChessPiece
  (trajectories [obj]
    "trajectories returns a map indexed by direction of trajectories"))

(defrecord King [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 8 planes (one for each direction)"
    (let [n          2 ; TODO change steps to be 1 and 7 and put +1 in trajs
          directions [[0 1]             ;up
                      [1 1]             ;up right
                      [-1 1]            ;up left
                      [0 -1]            ;down
                      [1 -1]            ;down right
                      [-1 -1]           ;down left
                      [1 0]             ;right
                      [-1 0]]]
      (trajs n directions))))


(defrecord Bishop [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7 x 4 planes"
    (let [n-cells   8
          direction [[1 1] [1 -1] [-1 1] [-1 -1]]
          ]
      (trajs n-cells direction ))))

(defrecord Knight [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 8 planes (one for each L)"
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
      (trajs steps directions))
    ))

(defrecord Rook [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7x4 planes. 1...7(number of cells) x 4(directions)"
    (let [steps      8
          directions [[0 1]             ;up
                      [0 -1]            ;down
                      [1 0]             ;right
                      [-1 0]            ;left
                      ]]
      (trajs steps directions))
    ))


(defrecord Pawn [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 2 planes (one for each L)"
    (let [steps      2
          directions [[0 (if (= (:color obj) "white") 1 -1)]        ; forward
                      [0 (if (= (:color obj) "white") 2 -2)]        ; double forward
                      [1 (if (= (:color obj) "white") 1 -1)]        ; diag right
                      [-1 (if (= (:color obj) "white") 1 -1)]        ; diag left
                      ]]
      (trajs steps directions))))

(defrecord Queen [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7x8 planes. 1...7(number of cells) x 8(directions)"
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
      (trajs steps directions))))

;;; board
(def board
  (atom
   {:turn "white"
    ;; white pieces
    [4 1] (map->King {:name "wking" :color "white"})
    ;; [3 0] (map->Queen {:name "wqueen" :color "white"})
    [5 0] (map->Bishop {:name "wbishop" :color "white"})
    [6 6] (map->Pawn {:name "bpawn" :color "black"})
    ;; [7 0] (map->Rook {:name "wrook" :color "white"})
    ;; [5 0] (map->Bishop {:name "wbishop" :color "white"})
    ;; [6 0] (map->Knight {:name "wknight" :color "white"})
    ;; black pieces
    [3 7] (map->King {:name "bking" :color "black"})
    }))

(defn get-pieces-names
  ([] (map :name (vals (dissoc @board :turn))))
  ([color] (map :name (vals (filter (fn [[k v]] (= (:color v) color)) @board)))))

(defn get-piece-map [name]
  (first (filter (fn [[k v]] (= (:name v) name)) @board))) ; TODO why do we need first?

(defn get-piece [name]
  (val (get-piece-map name)))

(defn get-piece-pos [name]
  (key (get-piece-map name)))

(defn get-piece-at-pos [pos]
  (:name (get @board pos)))

(defn get-piece-color [name]
  (:color (get-piece name)))

#_(get-pieces-of-color "white")

(defn is-pieces-turn? [name]
  (= (get-piece-color name) (:turn @board)))

(defn is-piece-selected? [name]
  (let [piece (get-piece name)]
    (get piece :selected)))

(defn abs [x]
  (if (> x 0) x (* -1 x)))

(defn legal-pawn-move? [name [to-x to-y]]
  "Pawns can only move vertically. It can move by 2 if it is in initial position"
  (let [[from-x from-y]      (get-piece-pos name)
        color                (get-piece-color name)
        is-vertical?         (= from-x to-x)
        advances-two?        (= (abs (- to-y from-y)) 2)
        is-pawn-initial-pos? (if (= color "white") (= from-y 1) (= from-y 6))
        is-legal-vertical?   (if advances-two? is-pawn-initial-pos? true)]
    (and is-vertical? is-legal-vertical?)))

#_(legal-pawn-move? "bpawn" [6 1])

(declare legal-king-move?)

(defn is-legal-move? [name [x y]]
  (and (not (contains? @board [x y]))
       (>= x  0) (< x  size)
       (>= y 0) (< y size)
       (if (re-find #"pawn" name) (legal-pawn-move? name [x y]) true)
       (if (re-find #"king" name) (legal-king-move? name [x y]) true)
       ))

(defn is-enemy-cell? [color cell]
  (let [cell-color (:color (get @board cell))]
    (and (some? cell-color) (not= color cell-color))))


;; TODO pawn en-passant
(defn does-piece-capture? [name cell]
  (let [piece-color (get-piece-color name)]
    (is-enemy-cell? piece-color cell)))

(defn get-legal-moves [name traj]
  "Trajectory is a sequence of moves in the same direction.
We first sort it and then 'cut' it when it goes off the board or hits a piece."
  (let [moves       (sort-by #(+ (abs (first %)) (abs (second %))) traj)
        legal-moves (take-while #(or (is-legal-move? name %) (does-piece-capture? name %)) moves)]
    legal-moves))

;; (get-legal-moves "white" [[7 3]])

(defn add-xy [[x y] [x-offset y-offset]]
  [(+ x x-offset) (+ y y-offset)])

(defn get-piece-moves [name]
  (let [[pos piece] (get-piece-map name)
        trajs       (trajectories piece)
        piece-trajs (map (fn [traj] (map (partial add-xy pos) traj)) trajs)
        legal-moves (map (partial get-legal-moves name) piece-trajs)]
    (apply concat legal-moves)))

;; check, checkmate

(defn is-check?
  ([] (let [color    (:turn @board)
            king-pos (get-piece-pos (str (first color) "king"))]
        (is-check? color king-pos)))
  ([color king-pos] (let [pieces       (get-pieces-names (if (= color "white") "black" "white"))
                          pieces-moves (apply concat (map get-piece-moves pieces))]
                      (some #(= king-pos %) pieces-moves))))

#_(is-check? "black" [3 6])

(defn legal-king-move? [name move]
  (let [color (get-piece-color name)]
    (if (= color (:turn @board))
      (not (is-check? color move))
      true)))

(defn is-checkmate? []
  (let [color       (:turn @board)
        king-moves  (get-piece-moves (str (first color) "king"))
        legal-moves (filter (partial legal-king-move? color) king-moves)]
    (= 0 (count legal-moves))))

;; interact with user selection

;; TODO unify with function above??
;; (defn is-legal-move? [name cell]
;;   (let [legal-moves (get-piece-moves name)]
;;     (some #(= cell %) legal-moves)))

#_(is-legal-move? "wbishop" [5 0])

(defn any-piece-selected? []
  (reduce (fn [res [pos piece]] (or res (get piece :selected))) false @board))

;; update board

(defn board-select! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (assoc piece :selected true))))

(defn board-unselect! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (dissoc piece :selected))))

(defn update-board! [name new-pos]
  (let [[previous-pos piece] (get-piece-map name)]
    (swap! board dissoc previous-pos)
    (swap! board assoc new-pos piece)))

(defn update-board-turn! []
  (let [new-color (if (= (:turn @board) "white") "black" "white")]
    (swap! board assoc :turn new-color)))
#_(update-board-turn!)

