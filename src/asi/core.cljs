(ns asi.core
  (:require [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn context 
  [width height]
  (let [target (.getElementById js/document "target")]
    (set! (. target -width) width)
    (set! (. target -height) height)   
    (.getContext target "2d")))

(defn fill-rect [ctx {:keys [x y w h]} c]
  (set! (. ctx -fillStyle) c)
  (.fillRect ctx x y w h))

(defprotocol Entity
  (tick [this])
  (render [this ctx]))

(defrecord Enemy [commands x y w h]
  Entity
  (render [this ctx] (fill-rect ctx this "#FF0"))
  (tick [this] this))

(defrecord Player [commands x y w h]
  Entity
  (render [this ctx] (fill-rect ctx this "#F00"))
  (tick [this] this))

(defn initial-state
  [commands ctx]
  { :entities (for [x (range 0 480 60)
                    y (range 0 240 60)]
                (Enemy. commands x y 20 20))
   :ctx ctx }) 

(defn game-action 
  [state action]
  (case action
    :tick (update-in state [:entities] #(map tick %1))
    :render (do
              (doseq [e (:entities state)] (render e (:ctx state)))
              state)))

(defn game
  [in ctx]
  (go (loop [state (initial-state in ctx)] 
        (recur (game-action state (<! in))))))

(defn start 
  []
  (let [ctx (context 640 480)
        commands (chan)
        instance (game commands ctx)]
   (js/setInterval #(put! commands :tick) (/ 1000.0 30.0)) 
   (js/setInterval #(put! commands :render) (/ 1000.0 30.0))))

(start)
