;; 2014-06-19
;; 0.75  930-1015

;; 2014-06-21
;; 2.75  9-1145

;; 2014-06-22
;; 0.25  230-245
;; 5  445-945
;; 0.25  11-1115

;; 2014-06-23
;; 0.75  730-815
;; 0.5  9-930

;; 2014-06-25
;; 0.5  10-1030

;; 2014-07-01
;; 0.25  1245-1
;; 1.5  830-10

;; 2014-07-02
;; 0.5  745-815
;; 0.75  845-930
;; 0.75  330-415

;; 2014-07-03
;; 115-2
;; 915-10

;; 0.75 + 2.75 + 0.25 + 5 + 0.25 + 0.75 + 0.5 + 0.5 + 0.25 + 1.5 + 0.5 + 0.75 + 0.75

(ns grover.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as s]
            [goog.events :as events]
            [goog.style :as style]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :as async :refer [>! <! put! take! chan close!]]
            [clojure.browser.repl :as repl])
  (:import [goog.events EventType]
           [goog.math Matrix]))

(enable-console-print!)

;; browser connected REPL at load http://localhost:9000/
(repl/connect "http://localhost:9000/repl")

(defn identity-matrix []
  (goog.math.Matrix. #js [#js [1 0 0]
                          #js [0 1 0]
                          #js [0 0 1]]))

(def app-state (atom {:views [{:view-transformation (identity-matrix)
                               :compositions [{:images [{:width 300
                                                         :height 300
                                                         :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"}]
                                               :children [{:transform "translate(159,166) scale(0.05)"
                                                           :images [{:width 300
                                                                     :height 300
                                                                     :xlink:href "https://upload.wikimedia.org/wikipedia/commons/b/b0/NewTux.svg"}]}]}
                                               {:images [{:width 300
                                                         :height 300
                                                         :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"}]
                                               :children [{:transform "translate(159,166) scale(0.05)"
                                                           :images [{:width 300
                                                                     :height 300
                                                                     :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"}]
                                                           :children [{:transform "translate(159,166) scale(0.05)"
                                                                       :images [{:width 300
                                                                                 :height 300
                                                                                 :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"}]
                                                                       :children [{:transform "translate(159,166) scale(0.05)"
                                                                                   :images [{:width 300
                                                                                             :height 300
                                                                                             :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"}]}]}]}]}]}
                              {:view-transformation (identity-matrix)
                               :compositions [{:images [{:width 300
                                                         :height 300
                                                         :xlink:href "http://scale.ydc2.yale.edu/iiif/970ef29a-5bf6-46a3-9a21-f27a3148c02f/full/full/0/native.jpg"}]
                                               :children [{:transform "translate(240,0)"
                                                           :images [{:width 300
                                                                     :height 300
                                                                     :xlink:href "http://scale.ydc2.yale.edu/iiif/d5f0b90f-3f83-455d-99c2-7f5dea0c998c/full/full/0/native.jpg"}]}]}
                                               ]}]}))

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

(defn draw-composition [{:keys [transform images children]}]
  (html
   [:g (when transform {:transform transform})
    ;(image {:width 300 :height 300 :xlink:href "https://mdn.mozillademos.org/files/2917/fxlogo.png"})
    (list (for [image-data images]
            (image image-data)))
    (for [child children]
      (draw-composition child))
    #_[:g {:transform "translate(159,166) scale(0.05)"}
     ;[:g {:transform "translate(200,0) scale(1)"}
     (image {:width 300 :height 300 :xlink:href "https://upload.wikimedia.org/wikipedia/commons/b/b0/NewTux.svg"})]]))

(defn view-pane-view [{:keys [composition view-transformation svg-view-transformation] :as app} owner]
  (reify
    om/IInitState
    (init-state [this]
                {:mouse-wheel-chan (chan)})

    om/IDidMount
    (did-mount [this]
               (let [mouse-pos-chan (async/map
                                     (fn [e] [(.-clientX e) (.-clientY e) e])
                                     [(listen (om/get-node owner :viewport) EventType.MOUSEMOVE)])
                     relative-mouse-pos-chan (async/map
                                              (fn [[x y]]
                                                (let [viewport-pos (style/getClientPosition (om/get-node owner :viewport))]
                                                  [(- x (.-x viewport-pos)) (- y (.-y viewport-pos))]))
                                              [mouse-pos-chan])
                     mouse-wheel-chan (om/get-state owner :mouse-wheel-chan)
                     mouse-down-chan (listen (om/get-node owner :viewport) "mousedown")
                     mouse-up-chan (listen (om/get-node owner :viewport) "mouseup")]

                 (go
                  (loop [mouse-pos [0 0]]
                    (let [[value ch] (alts! [mouse-pos-chan mouse-down-chan])]
                      (if (= ch mouse-pos-chan)
                        (recur value)
                        (do
                          (loop [last-mouse-pos mouse-pos]
                            (let [[value ch] (alts! [mouse-pos-chan mouse-up-chan])]
                              (when (= ch mouse-pos-chan)
                                (om/transact! app :view-transformation #(translate % (map - value last-mouse-pos)))
                                (recur value))))
                          (recur mouse-pos))))))

                 (go
                  (loop [mouse-pos [0 0]]
                    (let [[value ch] (alts! [relative-mouse-pos-chan mouse-wheel-chan])]
                      (cond (= ch mouse-wheel-chan) (do
                                                      (om/transact! app :view-transformation
                                                                        (fn [m]
                                                                          (let [[mouse-x mouse-y] mouse-pos]
                                                                            (-> m
                                                                                (translate [(- mouse-x) (- mouse-y)])
                                                                                (scale (- 1 (/ value 100)))
                                                                                (translate mouse-pos)))))
                                                      (recur mouse-pos))
                            :default (recur value)))))))

    om/IRenderState
    (render-state [this {:keys [mouse-wheel-chan] :as state}]
                  (let []
                    (html
                     [:span
                      [:svg {:ref :viewport
                             :width 600 :height 400 :style {:border "solid black 1px"}
                             :onWheel #(do
                                         (.preventDefault %)
                                         (put! mouse-wheel-chan (.-deltaY %)))}
                       [:g {:ref :view-transformation :transform (str "matrix(" (s/join \, svg-view-transformation) ")")}
                        (draw-composition composition)]]])))))

(defn app-view [{:keys [views view-transformation] :as app} owner]
  (reify
    om/IRenderState
    (render-state [this state]
                  (html
                   [:div
                    [:h2 "Grover"]
                    [:ul
                     [:li "About 150 lines of ClojureScript"]
                     [:li "No tests yet"]
                     [:li "Loading 'zoomed in' tiles doesn't work yet"]
                     [:li "You can drag with the mouse"]
                     [:li "Mousewheel controls zoom"]
                     [:li "Same zoom transformation can be used in mutiple viewports"]
                     [:li "Try zooming in at one mouse location and zoom out at another - the image doesn't jump around"]
                     [:li "Note that the fox (or red panda?) has two different images under its paw"]
                     [:li "It currently works well only in Chrome and Safari (IE is untested)"]]
                    (list
                     (for [{:keys [view-transformation compositions] :as view} views
                           composition compositions
                           :let [svg-view-transformation (apply mapcat list (take 2 (.toArray view-transformation)))
                                 app-with-svg-transform (assoc view
                                                          :svg-view-transformation svg-view-transformation
                                                          :composition composition)]]
                       (om/build view-pane-view app-with-svg-transform)))
                    ]))))

(om/root app-view app-state {:target (. js/document (getElementById "app"))})
