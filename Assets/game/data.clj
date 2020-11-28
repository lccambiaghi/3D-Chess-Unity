(ns game.data)

(def size 8)

;;; moves
(defn move-piece [[x-offset y-offset] [x y]]
  [(+ x x-offset) (+ y y-offset)])

(defn king-moves [pieces]
  "pieces is a sequence of [x y] cells, indicating where pieces are located.
Returns a sequence of 8 planes (one for each direction).
The output needs to be filled with zeros to create the actual matrix."
  (for [offset [[0 1] ;up
                [1 1] ;up right
                [-1 1] ;up left
                [0 -1] ;down
                [1 -1] ;down right
                [-1 -1] ;down left
                [1 0] ;right
                [-1 0]]] ;left
    (map (partial move-piece offset) pieces)))

(defn knight-moves [pieces]
  "Returns 8 planes (one for each direction)"
  (for [offset [[1 2] ;up right
                [-1 2]]] ;up left
    (map (partial move-piece offset) pieces)))

(defn bishop-moves [pieces]
  "Returns 7 x 2 planes"
  (for [n-cells   (range 1 8)
        direction [1 -1]
        :let      [offset [(* n-cells direction) (* n-cells direction)]]]
    (map (partial move-piece offset) pieces)))

(defn rook-moves [pieces]
  "Returns 7x4 planes. 1...7(number of cells) x 8(directions)"
  (for [n-cells   (range 1 8)
        direction [1 -1]
        mask      [[0 1][1 0]]
        :let      [offset [(* (first mask) n-cells direction) (* (second mask) n-cells direction)]]]
    (map (partial move-piece offset) pieces)))

;;; pieces
(defprotocol ChessPiece
  (trajectories [obj]))

(defrecord King [name color x y]
  ;; ChessPiece
  ;; (trajectories [x y]
  ;;   [{:x x :y (inc y)} {:x x :y (inc (inc y))}])
  )

(defrecord Bishop [name color x y]
  ;; ChessPiece
  ;; (trajectories [x y]
  ;;   [{:x x :y (inc y)} {:x x :y (inc (inc y))}])
  )

(defrecord Knight [name color x y]
  ;; ChessPiece
  ;; (trajectories [x y]
  ;;   [{:x x :y (inc y)} {:x x :y (inc (inc y))}])
  )

;;; board

(def board
  {[4 0] (map->King {:name "wking" :color "white"})
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

(defn get-piece-moves [name]
  (let [piece (filter (fn [[k v]] (= (:name v) name)) board)
        tracj])

  )


(comment
  (let [pieces (keys board)]
    (rook-moves pieces))

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
