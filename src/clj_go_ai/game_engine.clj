(ns clj-go-ai.game-engine
  (:require [clojure.set :as c-set]
            [clj-go-ai.ui.core :refer [keyword->color]]))

(defn init-state "The initial state of the game" [komi]
  {:player :black :black-groups #{} :white-groups #{}
   :black-captures 0 :white-captures komi :last-move []
   :black-groups-old #{} :white-groups-old #{}})

(defn neighbors [[x y] & {:keys [board-size] :or {board-size 19}}]
  (for [x-a [-1 0 1] y-a [-1 0 1]
        :let [n-x (+ x x-a) n-y (+ y y-a)]
        :when (and (not= (Math/abs x-a) (Math/abs y-a))
                   (<= 0 n-x) (< n-x board-size)
                   (<= 0 n-y) (< n-y board-size))]
    [n-x n-y]))

(defn color->keyword [color & {:keys [append] :or {append "-groups"}}]
  (keyword (str (name color) append)))

(defn groups-of [state color]
  ((color->keyword color) state))

(defn whose-turn [state]
  (:player state))

(defn last-turn [state]
  (:last-move state))

(defn enemy [color] (if (= color :black) :white :black))

(defn which-group-and-color [state pt]
  (let [blacks (filter #(contains? % pt) (groups-of state :black))
        whites (filter #(contains? % pt) (groups-of state :white))]
    (cond (not (empty? blacks)) [:black (first blacks)]
          (not (empty? whites)) [:white (first whites)]
          :else [:empty #{pt}])))

(defn liberties [state group]
  (count (filter #(= :empty (first %))
                 (map #(which-group-and-color state %)
                      (distinct (mapcat neighbors group))))))

(defn try-to-transform-state [state new-state]
  (if (every? #(= ((color->keyword %) new-state) ((color->keyword % :append "-groups-old") state))
              [:black :white])
    [:ko state]
    [:ok (merge-with (fn [old new] new)
                     (-> state
                         (dissoc :pass)
                         (assoc :black-groups-old (:black-groups state))
                         (assoc :white-groups-old (:white-groups state))
                         (assoc :player (enemy (:player state))))
                     new-state)]))
                    


(defn try-move [state move]
  (cond (let [[x y] move] (or (> 0 x) (>= x 19) (> 0 y) (>= y 19))) [:not-on-board state]
        (not= :empty (first (which-group-and-color state move))) [:non-empty-spot state]
        :else (let [curr-player (whose-turn state)
                    enemy (enemy curr-player)
                    move-neighbors (neighbors move)
                    neighboring-groups (map #(which-group-and-color state %) move-neighbors)
                    neighboring-friend-groups (map second (filter #(= curr-player (first %)) neighboring-groups))
                    neighboring-enemy-groups (map second (filter #(= enemy (first %)) neighboring-groups))
                    new-friend-group (reduce c-set/union (concat neighboring-friend-groups #{move}))
                    new-friend-groups (conj new-friend-group (reduce c-set/difference 
                                                                     (groups-of state curr-player)
                                                                     neighboring-friend-groups))
                    new-state {(color->keyword curr-player) new-friend-groups
                               (color->keyword enemy) (groups-of state enemy)
                               :last-move move}
                    captures (filter #(= 0 (liberties new-state %)) (groups-of state enemy))
                    capture-keyword (color->keyword curr-player :append "-captures")]
                (cond (not (empty? captures)) (try-to-transform-state 
                                                state 
                                                (-> new-state 
                                                    (assoc (color->keyword enemy)
                                                           (reduce c-set/difference
                                                                   (groups-of new-state enemy)
                                                                   captures))
                                                    (assoc capture-keyword
                                                           (reduce + (capture-keyword state) (map count captures)))))
                      (= 0 (liberties new-state new-friend-group)) [:suicidal state]
                      :else (try-to-transform-state state new-state)))))

(defn into-territories
  ([state] (loop [x 0 y 0 st {:black #{} :white #{} :empty #{}}]
             (cond (or (= 19 x) 
                       (= (* 19 19) (apply + (map count (vals st))))) st
                   (= 19 y) (let [[_ sp _] (into-territories state [(inc x) 0] st #{})]
                              (recur (inc x) 0 sp))
                   :else (let [[_ sp _] (into-territories state [x (inc y)] st #{})]
                           (recur x (inc y) sp)))))
  ([state curr-pt split cells-to-avoid]
    (let [in-split (filter #(contains? (second %) curr-pt))
          [color group] (which-group-and-color state curr-pt)]
      (cond (not (empty? in-split)) [(ffirst in-split) split]
            (not= :empty color) [color (assoc split color (c-set/union group (color split)))]
            :else (let [[f-c f-d] (reduce (fn [[cols sp-i av] neigh]
                                            (if (contains? cells-to-avoid neigh)
                                              [cols sp-i av]
                                              (let [[n-c n-d n-av] (into-territories neigh sp-i)]
                                                [(conj n-c cols) n-d n-av])))
                                          [#{} split (conj cells-to-avoid curr-pt)]
                                          (neighbors curr-pt))]
                    (if (< 1 (count f-c))
                      [:empty (assoc f-d :empty (conj (:empty f-d) curr-pt))]
                      [(first f-c) (assoc f-d (first f-c) (conj ((first f-c) f-d) curr-pt))]))))))

(defn chinese-score [state]
  (let [terrs (map (fn [[k v]] [k (count v)]) (into-territories state))]
    (reduce (fn [tr col] (assoc tr col 
                                (+ (tr col) 
                                   ((color->keyword col :append "-captures") state))))
            terrs [:black :white])))

(defn pass [state color]
  (if (:pass state false)
    (assoc state :pass true)
    (assoc state :game-over true)))

(defn resign [state color]
  (-> state
      (assoc :game-over true)
      (assoc :winner (enemy color))))

(defn is-end-game [state]
  (:game-over state false))

(defn map-for-ui [komi] [{:state-checker try-move
                          :whose-turn whose-turn
                          :init-state (init-state komi)
                          :pass pass
                          :resign resign
                          :is-end-game is-end-game}
                         {:white-getter (fn [state] (mapcat identity (groups-of state :white)))
                          :black-getter (fn [state] (mapcat identity (groups-of state :black)))
                          :last-getter (fn [state] [[(:last-move state) 
                                                     (keyword->color (enemy (:player state))) 
                                                     (keyword->color :red)]])}])
