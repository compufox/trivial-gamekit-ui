(defpackage :trivial-gamekit-ui-example
  (:nicknames :gamekit.ui.example)
  (:use :cl :gamekit :gamekit.ui)
  (:export :run))

(in-package :gamekit.ui.example)

;; create our game. the superclass WITH-UI gives us access to helper functions
(defgame example (with-ui) ()
  (:viewport-title "ui example"))

;; this function gets called after the game/state gets initialized
;;  so we use this function to set up our UI and handlers
(defmethod gamekit:post-initialize ((this example))
  (bind-cursor #'ui-mouse-handler)
  (bind-button :mouse-left :pressed
               #'ui-click-handler)
  (bind-button :mouse-left :released
               #'ui-release-handler)

  ;; a helper macro to create our UI and attach it to our game/state
  (initialize-ui this
    (:label1 (make-label "hello world!" :position (vec2 100 400)))
    (:label2 (make-label "hello larger world!" :position (vec2 100 300) :size 24))
    (:panel (make-panel :position (vec2 100 100) :width 200 :height 200 :rounding 10
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
   (:btn1 (make-button :position (vec2 350 150)
                       :on-click (lambda ()
                                   (setf (text (ui-element this :btn1)) "dynamic!"))
                       :label (make-label "click me!"
                                          :position (vec2 0 0)
                                          :size 48
                                          :color gamekit.colors:+white+)
                       :fill-color gamekit.colors:+cadetblue+
                       :hover-color gamekit.colors:+blue+
                       :rounding 7))
   (:bar (make-progress-bar 100 25 :position (vec2 300 400) :fill-color gamekit.colors:+pink+ :percent .5
                                   :rounding 5 :inner-color gamekit.colors:+steelblue+ :show-percent t))))

(defmethod gamekit:draw ((this example))
  ;; iterates over all our game/state UI elements and draws them
  (draw-ui this))
   
(defun run ()
  (start 'example))
