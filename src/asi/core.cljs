(ns asi.core
  (:require [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn context [width height]
  (let [target (.getElementById js/document "target")]
    [(.getContext target "2d") 
      (set! (. target -width) width)
      (set! (. target -height) height)]))

(defn game-action [state action]
  (.log js/console "zomg" action)
  state)

(defn game
  [in]
  (go (loop [state {}] 
        (recur (game-action state (<! in))))))

(defn start 
  []
  (let [ctx (context 640 480)
        commands (chan)
        instance (game commands)]
   (put! commands :start) ))


(start)
