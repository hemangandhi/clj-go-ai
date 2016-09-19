(ns clj-go-ai.core
  (:require [clj-go-ai.ui.core :as ui-core]
            [clj-go-ai.game-engine :as game-eng])
  (:gen-class))

(defn mapply [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (mapply ui-core/make-ui 600 
          (first (game-eng/map-for-ui 7.5)) (second (game-eng/map-for-ui 7.5))))
