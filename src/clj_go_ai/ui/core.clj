(ns clj-go-ai.ui.core
  (:import [javax.swing JPanel JFrame]
           [java.awt Graphics Color]))

(def keyword->color {:black Color/BLACK :white Color/WHITE :red Color/RED})

(defn get-coords 
  "Get the display coordinates for the board coordinates.
   [0 0] is the top left and [0 19] the bottom left."
  [board-coords offset]
  (map #(* offset (inc %)) board-coords))

(defn get-circle-params 
  "Gets the point and the radius."
  [board-coords offset diameter]
  (concat (map #(- % (/ diameter 2)) (get-coords board-coords offset)) [diameter]))

(defn draw-stone
  "Draws a stone. Highlights with inner-col by rendering a smaller circle of half the
   radius." 
  ([color board-coords offset graphics] (draw-stone color board-coords offset graphics color))
  ([color board-coords offset #^Graphics graphics inner-col]
    (let [stone-rad (/ offset 2)
          [b-x b-y r] (get-circle-params board-coords offset stone-rad)]
      (println (str "GRAPHICS:::" graphics))
      (. graphics setColor color)
      (. graphics fillOval b-x b-y stone-rad stone-rad)
      (let [[i-x i-y i-r] (get-circle-params board-coords offset (/ stone-rad 2))]
        (. graphics setColor inner-col)
        (. graphics fillOval i-x i-y i-r i-r)))))

(defn render-grid
  "Renders the grid for the game.
   The dimensions and star points can be configured
   optionally."
  ([offset graphics] (render-grid offset graphics {})) 
  ([offset graphics {:keys [board-len star-points]
                    :or {board-len 19
                         star-points [[3 3]  [9 3]  [15 3]
                                      [3 9]  [9 9]  [15 9]
                                      [3 15] [9 15] [15 15]]}}]
  (let [board-top 0]
    (. graphics setColor Color/BLACK)
    (dorun (map (fn [pt] (let [[x y r] (get-circle-params pt offset (/ offset 4))] 
                           (. graphics fillOval x y r r))) 
                star-points))
    (loop [curr-val 0]
      (if (< curr-val board-len)
        (let [[curr-coord top] (get-coords [curr-val board-top] offset)
              bottom (* offset board-len)] 
          (. graphics drawLine curr-coord top curr-coord bottom)
          (. graphics drawLine top curr-coord bottom curr-coord)
          (recur (inc curr-val))))))))

(defn draw-state 
  "Draws the board state. The getters are optional
   to allow for any sort of state to be passed in."
  [offset graphics state {:keys [black-getter white-getter last-getter]
                          :or {black-getter #(:black %)
                               white-getter #(:white %)
                               last-getter #(:last %)}}]
  (render-grid offset graphics)
  (dorun (map #(draw-stone Color/BLACK % offset graphics) (black-getter state)))
  (dorun (map #(draw-stone Color/WHITE % offset graphics) (white-getter state)))
  (dorun (map #(draw-stone (second %) (first %) offset graphics (nth % 2)) (last-getter state))))

(defn make-ui 
  "Makes a UI. The configurations must be passed in.

   The state-checker is expected to validate a state and return
   the validated state. This means captures should be performed here.
  
   whose-turn gets the current turn.
  
   init-state is expected to be the initial state.
  
   pass and resign handle passes and resignations. They are passed the state
   and the current color. They are expected to change the state.
  
   is-end-game returns whether the game is over.
  
   do-end-game captures the fact that the game is over.
  
   The move listeners (keyword arguments) are called (if provided) to prompt
   for a move. The return values are ignored.
  
   The returned map contains a function that takes a board coordinate and color and tries
   to move there (:try-to-move), a pointer to the JFrame (:frame), 
   whose turn it is, as a function (:turn) and a function to resign or pass
   that takes the color and :pass to pass otherwise, anything to resign (:try-to-resign-or-pass)."
  [dim {:keys [state-checker whose-turn init-state 
               pass resign is-end-game do-end-game]}
       & {:keys [black-getter white-getter last-getter
                 black-move-listener white-move-listener]
          :or {black-getter #(:black %)
               white-getter #(:white %)
               last-getter #(:last %)
               black-move-listener nil
               white-move-listener nil}}]
  (let [state (add-watch (ref init-state)
                         :end-game
                         (fn [key atom old new]
                           (if (is-end-game new)
                             (do-end-game new)
                             new)))
        offset (/ dim 20)
        getters {:black-getter black-getter
                 :white-getter white-getter
                 :last-getter last-getter}
        panel (proxy [JPanel] []
                (paintComponent [#^Graphics graphics]
                  (draw-state offset graphics @state getters)))
        frame (doto (JFrame. "Go Test!")
                (.setSize dim dim)
                (.add panel)
                (.setVisible true)
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))
        try-to-call-listener (fn [] (if (and (= (whose-turn @state) :white) 
                                             (not (nil? white-move-listener)))
                                      (white-move-listener @state)
                                      (when (not (nil? black-move-listener))
                                        (black-move-listener @state))))]
    {:try-to-move (fn [[x y] color]
                    (if (and (not (is-end-game @state)) (= color (whose-turn @state)))
                      (let [[chk new-st] (state-checker @state [x y])]
                        (if (= chk :ok)
                          (let [ret-val (dosync (ref-set state new-st))]
                            (. panel repaint)
                            (try-to-call-listener)
                            ret-val)
                          chk))
                      :not-your-turn))
     :frame frame
     :turn #(whose-turn @state)
     :try-to-resign-or-pass (fn [color pass-or-resign]
                              (if (and (not (is-end-game @state)) (= color (whose-turn @state)))
                                (if (= pass-or-resign :pass)
                                  (dosync (ref-set (pass @state color)))
                                  (dosync (ref-set (resign @state color))))
                                :not-your-turn))}))
