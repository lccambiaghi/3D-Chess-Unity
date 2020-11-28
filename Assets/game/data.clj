(ns game.data)

(def size 8)

;; TODO: in get-legal-moves, it is now easy to filter away the invalid part of each trajectory by sorting
;; TODO: maybe extract a generic function and only parametrize n-cells, directions, etc.?

;;; pieces
(defn trajectories [n-cells directions masks]
  (for [n-cells   (range 1 8)
        direction [[1 1] [1 -1] [-1 1] [-1 -1]]
        :let      [offset {direction [(* n-cells (first direction)) (* n-cells (second direction))]}]]
    (apply merge-with into offset)))

(defprotocol ChessPiece
  (trajectories [obj]
    "trajectories returns a map indexed by direction of trajectories"))

(defrecord King [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 8 planes (one for each direction)"
    [[0 1]   ;up
     [1 1]   ;up right
     [-1 1]  ;up left
     [0 -1]  ;down
     [1 -1]  ;down right
     [-1 -1] ;down left
     [1 0]   ;right
     [-1 0]]))

(defrecord Bishop [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7 x 4 planes"
    (for [n-cells   (range 1 8)
          direction [[1 1] [1 -1] [-1 1] [-1 -1]]
          :let      [offset {direction [(* n-cells (first direction)) (* n-cells (second direction))]}]]
      (apply merge-with into offset)
      )))

(defrecord Knight [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 8 planes (one for each L)"
    [[1 2] ;up right
     [-1 2] ; up left
     [1 -2] ; bottom right
     [-1 -2] ; bottom left
     [2 1] ; right up
     [2 -1] ; right down
     [-2 1] ; left up
     [-2 -1] ; left down
     ]))

(defrecord Rook [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7x4 planes. 1...7(number of cells) x 4(directions)"
    (for [n-cells   (range 1 8)
          direction [1 -1]
          mask      [[0 1][1 0]]
          :let      [offset [(* (first mask) n-cells direction) (* (second mask) n-cells direction)]]]
      offset)))

(defrecord Pawn [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 2 planes (one for each L)"
    [[0 1] ;up
     [0 2] ; up up
     ]))

(defrecord Queen [name color x y]
  ChessPiece
  (trajectories [obj]
    "Returns 7x8 planes. 1...7(number of cells) x 8(directions)"
    (for [n-cells   (range 1 8)
          direction [[1 1] [1 -1] [-1 1] [-1 -1]]
          mask      [[0 1][1 0]]
          :let      [offset [(* (first mask) n-cells (first direction)) (* (second mask) n-cells (second direction))]]]
      offset)))

;;; board
(def board
  {[4 1] (map->King {:name "wking" :color "white"})
   ;; [3 0] (map->Queen {:name "wqueen" :color "white"})
   [5 0] (map->Bishop {:name "wbishop" :color "white"})
   [7 0] (map->Rook {:name "wrook" :color "white"})
   ;; [5 0] (map->Bishop {:name "wbishop" :color "white"})
   ;; [6 0] (map->Knight {:name "wknight" :color "white"})
   ;; [3 7] (map->King {:name "bking" :color "black"})
   })

(defn get-pieces-names []
  (map :name (vals board)))

(defn get-piece-pos [name]
  (-> (filter (fn [[k v]] (= (:name v) name)) board)
      first
      key))

(defn move-piece [[x y] [x-offset y-offset]]
  [(+ x x-offset) (+ y y-offset)])

(defn legal-move? [[x y]]
  "Legal if cell is not occupied and cell is in the board.
Taken trajectories, find the first occupied cell in the trajectory and only keep those"
  (and (not (contains? board [x y]))
       (>= x 0) (< x size)
       (>= y 0) (< y size))
  )

(defn get-piece-moves [name]
  (let [[pos piece]  (first (filter (fn [[k v]] (= (:name v) name)) board))
        trajectories (trajectories piece)
        moves        (map (partial move-piece pos) trajectories)
        legal-moves  (filter legal-move? moves)]
    legal-moves
    ))


(comment
  (get-piece-pos "wking")
  (partition-by first (sort-by first (get-piece-moves "wrook")))

  (group-by second (get-piece-moves "wrook"))

  (group-by #(+ (first %) (second %)) (get-piece-moves "wbishop"))


  (count (trajectories (map->Queen {:name "wqueen" :color "white"})))

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
