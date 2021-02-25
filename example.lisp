(defpackage :trivial-gamekit-ui-example
  (:nicknames :gamekit.ui.example)
  (:use :cl :gamekit :gamekit.ui)
  (:export :run))

(in-package :gamekit.ui.example)

(defgame example () ()
  (:viewport-title "ui example"))

(defun click-me-callback ()
  (setf (text *btn1*)
        "dynamic!"))

;; if you were loading custom assets like images/fonts
;;  you'll want to shove all of these into some kind of
;;  post-initialize function or method
(defvar *label1* (make-label "hello world!" :position (vec2 100 400)))
(defvar *label2* (make-label "hello larger world!" :position (vec2 100 300) :size 24))
(defparameter *panel* (make-panel :position (vec2 100 100) :width 200 :height 200 :rounding 10
                                  :children (list (make-label "This is a panel!"
                                                              :position (vec2 25 150)
                                                              :size 20
                                                              :color gamekit.colors:+white+)
                                                  (make-button :position (vec2 25 100)
                                                               :label (make-label "Panel Button!"
                                                                                  :position (vec2 0 0)
                                                                                  :size 20
                                                                                  :color gamekit.colors:+white+)
                                                               :fill-color gamekit.colors:+deeppink+
                                                               :hover-color gamekit.colors:+hotpink+
                                                               :pressed-color gamekit.colors:+red+))))
(defparameter *btn1* (make-button :position (vec2 350 150)
                                  :on-click #'click-me-callback
                                  :label (make-label "click me!"
                                                     :position (vec2 0 0)
                                                     :size 48
                                                     :color gamekit.colors:+white+)
                                  :fill-color gamekit.colors:+cadetblue+
                                  :hover-color gamekit.colors:+blue+
                                  :rounding 7))

(defmethod gamekit:draw ((this example))
  (draw-widget *panel*)
  (draw-widget *label1*)
  (draw-widget *label2*)
  (draw-widget *btn1*))
   
(defun run ()
  (start 'example)
  (bind-cursor #'ui-mouse-handler)
  (bind-button :mouse-left :pressed
               #'ui-click-handler)
  (bind-button :mouse-left :released
               #'ui-release-handler))
  
