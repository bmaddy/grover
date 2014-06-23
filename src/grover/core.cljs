;; 2014-06-19
;; 0.75  930-1015

;; 2014-06-21
;; 2.75  9-1145

;; 2014-06-22
;; 0.25  230-245
;; 5  445-945
;; 0.25  11-1115

;; 2014-06-23
;; 730-815
;; 9-930

(ns grover.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as s]
            [goog.events :as events]
            [goog.style :as style]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [>! <! put! take! chan close!]])
  (:import [goog.events EventType]
           [goog.math Matrix]))

(enable-console-print!)

(def app-state (atom {:zoom 1}))

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type #(put! out %))
    out))

(defn image [attrs]
  (html
   [:g {:dangerouslySetInnerHTML {:__html (str "<image "
                                              (reduce (fn [s [k v]] (str s \space (name k) \= \" v \")) "" attrs)
                                              " />")}}]))

(defn scale [m amount]
  (.multiply (goog.math.Matrix. #js [#js [amount 0 0]
                                     #js [0 amount 0]
                                     #js [0 0 1]])
             m))

(defn translate [m [x y]]
  (.multiply (goog.math.Matrix. #js [#js [1 0 x]
                                     #js [0 1 y]
                                     #js [0 0 1]])
             m))

(defn app-view [app owner]
  (reify
    om/IInitState
    (init-state [this]
                {:mouse-wheel-chan (chan)
                 :view-transformation (goog.math.Matrix. #js [#js [1 0 0]
                                                              #js [0 1 0]
                                                              #js [0 0 1]])})

    om/IDidMount
    (did-mount [this]
               (let [mouse-pos-chan (async/map
                                     (fn [e] [(.-clientX e) (.-clientY e)])
                                     [(listen (om/get-node owner :viewport) EventType.MOUSEMOVE)])
                     relative-mouse-pos-chan (async/map
                                              (fn [[x y]]
                                                (let [viewport-pos (style/getClientPosition (om/get-node owner :viewport))]
                                                  [(- x (.-x viewport-pos)) (- y (.-y viewport-pos))]))
                                              [mouse-pos-chan])
                     mouse-wheel-chan (om/get-state owner :mouse-wheel-chan)]
                 (go
                  (loop [mouse-pos [0 0]]
                    (let [[value ch] (alts! [relative-mouse-pos-chan mouse-wheel-chan])]
                      (cond (= ch mouse-wheel-chan) (do
                                                      (om/update-state! owner :view-transformation
                                                                        (fn [m]
                                                                          (let [[mouse-x mouse-y] mouse-pos
                                                                                offset-x (.getValueAt m 0 2)
                                                                                offset-y (.getValueAt m 1 2)]
                                                                            (-> m
                                                                                (translate [(- mouse-x) (- mouse-y)])
                                                                                (scale (- 1 (/ value 100)))
                                                                                (translate mouse-pos)))))
                                                      (recur mouse-pos))
                            :default (recur value)))))))

    om/IRenderState
    (render-state [this {:keys [view-transformation zoom zoom-offset mouse-wheel-chan] :as state}]
                  (let [[zoom-offset-x zoom-offset-y] zoom-offset
                        svg-view-transformation (apply mapcat list (take 2 (.toArray view-transformation)))]
                    ;(println state)
                    (html
                     [:div
                      [:h2 "Grover"]
                      [:svg {:ref :viewport
                             :width 800 :height 600 :style {:border "solid black 1px"}
                             :onWheel #(do
                                         (.preventDefault %)
                                         (put! mouse-wheel-chan (.-deltaY %)))}
                       [:g {:ref :view-transformation :transform (str "matrix(" (s/join \, svg-view-transformation) ")")}
                        (image {:width 300 :height 300 :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"})
                        [:g {:transform "translate(159,166) scale(0.05)"}
                         (image {:width 300 :height 300 :xlink:href "https://upload.wikimedia.org/wikipedia/commons/b/b0/NewTux.svg"})]]]])))))

(om/root app-view app-state {:target (. js/document (getElementById "app"))})
