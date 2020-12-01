(ns game.data)

(def size 8)

;; TODO: in get-legal-moves, it is now easy to filter away the invalid part of each trajectory by sorting
;; TODO: maybe extract a generic function and only parametrize n-cells, directions, etc.?

;;; pieces
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
    (let [steps      2
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
    (let [steps      8
          directions [[0 1] ;up
                      [0 2] ; up up
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
   {[4 1] (map->King {:name "wking" :color "white"})
    ;; [3 0] (map->Queen {:name "wqueen" :color "white"})
    [5 0] (map->Bishop {:name "wbishop" :color "white"})
    [7 2] (map->Pawn {:name "bpawn" :color "black"})
    ;; [7 0] (map->Rook {:name "wrook" :color "white"})
    ;; [5 0] (map->Bishop {:name "wbishop" :color "white"})
    ;; [6 0] (map->Knight {:name "wknight" :color "white"})
    ;; [3 7] (map->King {:name "bking" :color "black"})
    }))

(defn get-pieces-names []
  (map :name (vals @board)))

(defn get-piece-map [name]
  (first (filter (fn [[k v]] (= (:name v) name)) @board))) ; TODO why do we need first?

(defn get-piece [name]
  (val (get-piece-map name)))

(defn get-piece-pos [name]
  (key (get-piece-map name)))

(defn get-piece-color [name]
  (:color (get-piece name)))

#_(get-piece-pos "wbishop")
#_(get-piece-color "wking")
#_(get-piece "wking")

(defn abs [x]
  (if (> x 0) x (* -1 x)))

(defn legal-cell? [[x y]]
  (and (not (contains? @board [x y]))
       (>= x  0) (< x  size)
       (>= y 0) (< y size)))

(defn capture? [color cell]
  (let [cell-color (:color (get @board cell))]
    (and (some? cell-color) (not= color cell-color))))

(defn legal-move? [color cell]
  (let [cell-color (:color (get @board cell))
        capture?   (and (some? cell-color) (not= color cell-color))
        legal?     (and (not (contains? @board cell))
                        (>= (first cell)  0) (< (first cell)  size)
                        (>= (second cell) 0) (< (second cell) size))]
    (or legal? capture?)))



(defn filter-traj [color traj]
  "Trajectory is a sequence of moves in the same direction.
We first sort it and then 'cut' it when it goes off the baord or hits a piece."
  (let [traj-sorted (sort-by #(+ (abs (first %)) (abs (second %))) traj)
        traj-legal  (take-while #(or (legal-cell? %) (capture? color %)) traj-sorted)]
    traj-legal))

;; (filter-traj "white" [[7 3]])

(defn add-xy [[x y] [x-offset y-offset]]
  [(+ x x-offset) (+ y y-offset)])

(defn get-piece-moves [name]
  (let [[pos piece] (get-piece-map name)
        color       (:color piece)
        trajs       (trajectories piece)
        piece-trajs (map (fn [traj] (map (partial add-xy pos) traj)) trajs)
        legal-moves (map (partial filter-traj color) piece-trajs)]
    (apply concat legal-moves)))

(defn is-legal-move? [name cell]
  (let [legal-moves (get-piece-moves name)]
    (some #(= cell %) legal-moves)))

#_(is-legal-move? "wbishop" [5 0])

(defn update-board! [name new-pos]
  (let [[previous-pos piece] (get-piece-map name)]
    (swap! board dissoc previous-pos)
    (swap! board assoc new-pos piece)))

(defn is-piece-selected? [name]
  (let [piece (get-piece name)]
    (get piece :selected)))

(defn any-piece-selected? []
  (reduce (fn [res [pos piece]] (or res (get piece :selected))) false @board))

(defn board-select! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (assoc piece :selected true))))

(defn board-unselect! [name]
  (let [[pos piece] (get-piece-map name)]
    (swap! board assoc pos (dissoc piece :selected))))

(comment
  (get-piece-pos "wking")
  (partition-by first (sort-by first (get-piece-moves "wrook")))

  (trajectories (map->Bishop {:name "wbishop" :color "white"}))

  (group-by second (get-piece-moves "wrook"))

  (count (get-piece-moves "wbishop"))


  (count (trajectories (map->Queen {:name "wqueen" :color "white"})))

  (let [a [[1 4] [1 2] [1 3]]
        b [[2 -2] [1 -1] [3 -3]]
        c [[-1 1] [-2 2] [-3 3]]]
    (sort-by #(+ (abs (first %)) (abs (second %))) c))

  ;;; matrix
  (nth (repeat size (repeat size 0)) 2)

  (def matrix
    [[1 2 3]
     [4 5 6]
     [7 8 9]])

  (get-in matrix [1 2])

  (assoc-in matrix [1 2] 'x)

  (subvec matrix 0 2)
  )
