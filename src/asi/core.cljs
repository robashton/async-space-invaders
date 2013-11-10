(ns asi.core
  (:require [cljs.core.async :as async :refer [<! >! chan put!]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(def comms (chan))

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
(defn rect-right [rect] (+ (:x rect) (:w rect)))
(defn rect-bottom [rect] (+ (:y rect) (:h rect)))

(defn entity [id rect color]
  (-> rect
    (assoc :color color)
    (assoc :id id)))

(defn enemy [x y w h]
  (-> (entity (str "enemy" x y) (rect x y w h) "#FF0")
    (assoc :type :enemy)
    (assoc :velx 1)))

(defn player [x y w h]
  (-> (entity :player (rect x y w h) "#F00")
   (assoc :type :player)))

(defn bullet [x y w h]
  (-> (entity (str "bullet" (rand)) (rect x y w h) "#000")
    (assoc :type :bullet)
    (assoc :vely -5)))

(defn apply-physics [entity]
  (-> entity
    (update-in [:x] #(+ %1 (:velx entity)))
    (update-in [:y] #(+ %1 (:vely entity)))))

(defn add-entity [entities e]
  (assoc entities (:id e) e))

(defn player? [e] (= :player (:type e)))
(defn enemy? [e] (= :enemy (:type e)))
(defn bullet? [e] (= :bullet (:type e)))

(defn player-left [entities]
  (assoc-in entities [:player :velx] -1))

(defn player-right [entities]
  (assoc-in entities [:player :velx] 1))

(defn player-halt [entities]
  (assoc-in entities [:player :velx] 0))

(defn player-fire [entities]
  (add-entity entities
    (let [{:keys [x y]} (entities :player)] 
      (bullet x y 5 5))))

(defn initial-enemies []
  (for [x (range 0 480 60) y (range 0 240 60)]
                             (enemy x y 20 20)))

(defn initial-scene []
  (into {} (for [e (conj (initial-enemies)
                         (player 200 430 20 20))] [(:id e) e]))
                         (firing-guard))

(defn min-enemy-left [enemies]
  (apply min (map :x enemies)))

(defn max-enemy-right [enemies]
  (apply max (map rect-right enemies)))

(defn enemies-at-border [enemies]
  (or (<= 640 (max-enemy-right enemies)) 
      (>= 0 (min-enemy-left enemies))))

(defn destroy-enemy [id entities]
  (dissoc entities id))

(defn destroy-bullet [id entities]
  (dissoc entities id))

(defn next-enemy-level [e]
  (-> e
    (update-in [:velx] (partial - 0))
    (update-in [:y] (partial + 10))))

(defn collisions-in [entities]
  (filter 
      boolean
      (for [[_ one] entities
            [_ two] entities]
           (cond
             (= one two) nil
             (< (rect-right one) (:x two)) nil
             (> (:x one) (rect-right two)) nil
             (< (rect-bottom one) (:y two)) nil
             (> (:y one) (rect-bottom two)) nil
             :else [one two])))) 

(defn check-collisions [entities]
  (doseq [[one two] (collisions-in entities)]
      (case [(:type one) (:type two)]
        [:bullet :enemy] (put! comms (partial destroy-enemy (:id two)))
        [:enemy :bullet] (put! comms (partial destroy-bullet (:id two)))
        nil))
  entities)

(defn check-enemy-directions [entities]
  (if (enemies-at-border (filter enemy? (map val entities)))
    (into entities (for [[i e] entities] [i (next-enemy-level e)])) 
    entities))

(defn tick [entities]
  (-> entities 
    (into (for [[i e] entities] [i (apply-physics e)])) 
    (check-enemy-directions)
    (check-collisions)))

(defn render [ctx entities]
  (clear ctx)
  (doseq [e (map val entities)] (draw-entity ctx e)))

(defn game []
  (go 
    (loop [entities (initial-scene)]
      (if-let [action (<! comms)]
        (recur (or (action entities) entities))))))

(defn on-key-down [e]
  (case (. e -keyCode)
    37 (put! comms player-left)
    39 (put! comms player-right)
    32 (put! comms player-fire)
    nil))

(defn on-key-up [e]
  (case (. e -keyCode)
    37 (put! comms player-halt)
    39 (put! comms player-halt)
    nil))

(defn hook-input-events []
  (.addEventListener js/document "keydown" on-key-down)
  (.addEventListener js/document "keyup" on-key-up))

(defn start []
  (let [ctx (context 640 480)
        instance (game)]
   (hook-input-events)
   (js/setInterval #(put! comms tick) (/ 1000.0 30.0)) 
   (js/setInterval #(put! comms (partial render ctx)) (/ 1000.0 30.0))))

(start)
