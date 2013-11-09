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

(defn enemy [out x y w h]
 (let [in (chan)]
   (go (loop [state {:x x :y y :w w :h h}]
         (if-let [action (<! in)]
           (recur (case (:cmd action)
            :render (do (fill-rect (:ctx action) state "#FF0") state)
            :tick state)))))
   {:channel in :id (str "enemy" x y) }))

(defn player [out x y w h]
 (let [in (chan)]
   (go (loop [state {:x x :y y :w w :h h}]
         (if-let [action (<! in)]
           (recur (case (:cmd action)
            :render (do (fill-rect (:ctx action) state "#F00") state)
            :tick state)))))
   {:channel in :id "player"}))


(defn initial-state
  [out ctx]
  { :entities (into {} 
                (for [e 
                  (conj (for [x (range 0 480 60)
                        y (range 0 240 60)]
                      (enemy out x y 20 20))
                        (player out 200 430 20 20))] [(:id e) e]))
    :ctx ctx })

(defn broadcast [scene cmd & data]
  (doseq [[id {ch :channel}] (:entities scene)] (put! ch (assoc (apply hash-map data) :cmd cmd)))
  scene)

(defn game-action 
  [scene action]
  (case action
    :tick (broadcast scene :tick)
    :render (broadcast scene :render :ctx (:ctx scene))))

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
