;; 2014-06-19
;; 930-1015
;; 2014-06-21
;; 9-1145
;; 2014-06-22
;; 230-245
;; 445-
(ns mantis.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as s]
            [goog.events :as events]
            [goog.style :as style]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [>! <! put! take! chan close!]])
  (:import [goog.events EventType]))

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

(defn app-view [app owner]
  (reify
    om/IInitState
    (init-state [this]
                {:zoom-offset [0 0]
                 :mouse-wheel-chan (chan)
                 :zoom 1})

    om/IDidMount
    (did-mount [this]
               (go (let [mouse-pos-chan (async/map
                                         (fn [e] [(.-clientX e) (.-clientY e)])
                                         [(listen (om/get-node owner :viewport) EventType.MOUSEMOVE)])
                         relative-mouse-pos-chan (async/map
                                                  (fn [[x y]]
                                                    (let [viewport-pos (style/getClientPosition (om/get-node owner :translate-b))]
                                                      [(- x (.-x viewport-pos)) (- y (.-y viewport-pos))]))
                                                  [mouse-pos-chan])
                         mouse-wheel-chan (om/get-state owner :mouse-wheel-chan)]
                     (loop [mouse-pos [0 0]]
                       (let [[value ch] (alts! [mouse-pos-chan mouse-wheel-chan])]
                         (cond (= ch mouse-wheel-chan) (do
                                                         ;(println [:zoom mouse-pos value])
                                                         (om/update-state! owner (fn [s]
                                                                                   (-> s
                                                                                       (assoc :zoom-offset mouse-pos)
                                                                                       (update-in [:zoom] #(- % (/ value 100))))))
                                                         (recur mouse-pos))
                               :default (recur value)))))))

    om/IRenderState
    (render-state [this {:keys [zoom zoom-offset mouse-wheel-chan] :as state}]
                  (let [[zoom-offset-x zoom-offset-y] zoom-offset]
                    (println state)
                    (when-not (= zoom 1)
                      (println (style/getPosition (om/get-node owner :viewport)))
                      (println (style/getClientPosition (om/get-node owner :viewport))))
                    (html
                     [:div
                      [:h2 "Mantis Shrimp"]
                      [:svg {:ref :viewport
                             :width 800 :height 600 :style {:border "solid black 1px"}
                             :onWheel #(do
                                         (.preventDefault %)
                                         (put! mouse-wheel-chan (.-deltaY %)))}
                       [:g {:ref :translate-a :transform (str "translate(" zoom-offset-x "," zoom-offset-y")")}
                        [:g {:ref :scale :transform (str "scale(" zoom ")")}
                         [:g {:ref :translate-b :transform (str "translate(" (- zoom-offset-x) "," (- zoom-offset-y) ")")}
                          (image {:width 300 :height 300 :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"})]]]]])))))

(om/root app-view app-state {:target (. js/document (getElementById "app"))})
