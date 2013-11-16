(ns asi.core
  (:require [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))


;; TODO: Can I channel events to a separate rendering system? (three js)
(defn context 
  [width height]
  (let [target (.getElementById js/document "target")]
    (set! (. target -width) width)
    (set! (. target -height) height)   
    (.getContext target "2d")))

(defn clear [ctx]
  (set! (. ctx -fillStyle) "#FFF")
  (.fillRect ctx 0 0 640 480))

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
   (go (loop [state {:x x :y y :w w :h h }]
         (if-let [msg (<! in)]
           (recur (case (:cmd msg)
            :render (do (fill-rect (:ctx msg) state "#F00") state)
            :change-dir (assoc state :dir (:dir msg))
            :tick (case (:dir state)
                    :left (update-in state [:x] dec)
                    :right (update-in state [:x] inc)
                    state))))))
   {:channel in :id :player}))

(defn initial-state
  [out ctx]
  { :entities (into {} 
                (for [e 
                  (conj (for [x (range 0 480 60)
                        y (range 0 240 60)]
                      (enemy out x y 20 20))
                        (player out 200 430 20 20))] [(:id e) e]))
    :ctx ctx })

(defn broadcast 
  [scene msg]
  (doseq [[id {ch :channel}] (:entities scene)] (put! ch msg))
  scene)

(defn message
  [scene id msg]
  (if-let [ch (get-in scene [:entities id :channel])] (put! ch msg))
  scene)

(defn game-action 
  [scene msg]
  (if-let [id (:id msg)]
    (message scene id msg)
    (broadcast scene msg)))

(defn game
  [in ctx]
  (go (loop [state (initial-state in ctx)] 
        (recur (game-action state (<! in))))))

(defn send-to 
  [port cmd & data]
  (put! port (assoc (apply hash-map data) :cmd cmd)))

(defn on-key-down [commands]
  (fn [e] 
    (case (. e -keyCode)
      37 (send-to commands :change-dir :id :player :dir :left)
      39 (send-to commands :change-dir :id :player :dir :right))))

(defn on-key-up [commands]
  (fn [e] 
    (case (. e -keyCode)
      37 (send-to commands :change-dir :id :player :dir :none) 
      39 (send-to commands :change-dir :id :player :dir :none))))

(defn hook-input-events [commands]
  (.addEventListener js/document "keydown" (on-key-down commands))
  (.addEventListener js/document "keyup" (on-key-up commands)))

(defn start []
  (let [ctx (context 640 480)
        commands (chan)
        instance (game commands ctx)]
   (hook-input-events commands)
   (js/setInterval #(send-to commands :tick) (/ 1000.0 30.0)) 
   (js/setInterval #(do (clear ctx) (send-to commands :render :ctx ctx)) (/ 1000.0 30.0))))

(start)
