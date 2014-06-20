(ns mantis.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(def app-state (atom {:text "Hello world!"}))

(om/root
  (fn [app owner]
    (html
     [:h1 (:text app)]))
  app-state
  {:target (. js/document (getElementById "app"))})
