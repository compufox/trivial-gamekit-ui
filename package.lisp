;;;; package.lisp

(defpackage #:trivial-gamekit-ui
  (:use #:cl)
  (:nicknames :gamekit.ui)
  (:import-from :gamekit :vec2 :x :y)
  (:export :make-label :make-button :make-panel :make-progress-bar
           :text :size :widget-position :text-color :image 
           :stroke :hover-color :pressed-color
           :on-click :stroke-thickness :percent
           :hover-image :pressed-image :stroke-color
           :children :width :height :label :rounding
           :justification :render

           :text-width :text-height :text-dimensions
           :draw-widget

           :ui-mouse-handler :ui-click-handler :ui-release-handler
           :with-ui :initialize-ui :draw-ui :ui-element))

(in-package :gamekit.ui)

(defvar *font-cache* (make-hash-table))
(defvar *mouse-position* (vec2 0 0))
(defvar *mouse-clicked* nil)
