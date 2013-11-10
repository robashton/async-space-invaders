(ns asi.core
  (:require [cljs.core.async :as async :refer [<! >! chan put!]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn context 
  [width height]
  (let [target (.getElementById js/document "target")]
    (set! (. target -width) width)
    (set! (. target -height) height)   
    (.getContext target "2d")))

(defn clear [ctx]
  (set! (. ctx -fillStyle) "#FFF")
  (.fillRect ctx 0 0 640 480))

(defn draw-entity [ctx {:keys [x y w h color]}]
  (set! (. ctx -fillStyle) color)
  (.fillRect ctx x y w h color))

(defn rect [x y w h] { :x x :y y :w w :h h})

(defn entity [id rect color]
  (-> rect
    (assoc :color color)
    (assoc :id id)))

(defn apply-physics [entity]
  (update-in entity [:x] #(+ %1 (:velx entity))))

(defn enemy [x y w h]
  (-> (entity (str "enemy" x y) (rect x y w h) "#FF0"))
    (assoc :type :enemy))

(defn player [x y w h]
  (-> (entity :player (rect x y w h) "#F00"))
  (assoc :type :player))

(defn player? [e] (= :player (:type e)))
(defn enemy? [e] (= :enemy (:type e)))

(defn player-left [scene]
  (assoc-in scene [:entities :player :velx] -1))

(defn player-right [scene]
  (assoc-in scene [:entities :player :velx] 1))

(defn player-halt [scene]
  (assoc-in scene [:entities :player :velx] 0))

(defn enemy-direction [direction {:keys [entities] :as scene}]
  (assoc scene :entities 
      (into entities (for [[i e] (filter enemy? entities)] 
                       (assoc e :velx direction)))))

(defn initial-scene
  [out ctx]
  { :entities (into {} 
                (for [e 
                  (conj (for [x (range 0 480 60) y (range 0 240 60)]
                             (enemy x y 20 20))
                        (player 200 430 20 20))] [(:id e) e])) })

(defn tick [{:keys [entities] :as scene}]
  (assoc scene :entities (into entities (for [[i e] entities] [i (apply-physics e)]))))

(defn render [ctx {:keys [entities]}]
  (clear ctx)
  (doseq [e (map val entities)] (draw-entity ctx e)))

(defn game
  [in ctx]
  (go (loop [scene (initial-scene in ctx)] 
        (if-let [action (<! in)]
          (recur (or (action scene) scene))))))

(defn on-key-down [commands]
  (fn [e] 
    (case (. e -keyCode)
      37 (put! commands player-left)
      39 (put! commands player-right)
      nil)))

(defn on-key-up [commands]
  (fn [e] 
    (case (. e -keyCode)
      37 (put! commands player-halt)
      39 (put! commands player-halt)
      nil)))

(defn hook-input-events [commands]
  (.addEventListener js/document "keydown" (on-key-down commands))
  (.addEventListener js/document "keyup" (on-key-up commands)))

(defn start []
  (let [ctx (context 640 480)
        commands (chan)
        instance (game commands ctx)]
   (hook-input-events commands)
   (js/setInterval #(put! commands tick) (/ 1000.0 30.0)) 
   (js/setInterval #(put! commands (partial render ctx)) (/ 1000.0 30.0))))

(start)
