(in-package :gamekit.ui)

(defun make-label (text &key (position (vec2 0 0)) (font :default) (size 12) (color gamekit.colors:+black+)
                          (justification :left))
  "creates and returns a label object"
  (find-or-create-font font size)
  (make-instance 'label :text text :position position
                        :text-color color
                        :font font
                        :justification justification
                        :size size))

(defun make-button (&key (position (vec2 0 0)) label (fill-color gamekit.colors:+black+)
                      (hover-color gamekit.colors:+black+)
                      (pressed-color gamekit.colors:+black+) (stroke-color gamekit.colors:+transparent+)
                      (stroke-thickness 0) (rounding 0)
                      image hover-image pressed-image
                      on-click)
  "creates and returns a button"
  (if image
      (make-instance 'image-button :image image :hover-image (or hover-image image)
                                   :pressed-image (or pressed-image image) :position position
                                   :on-click on-click)
      (make-instance 'button :label label :position position
                             :fill-color fill-color :hover-color hover-color :pressed-color pressed-color
                             :stroke-color stroke-color :stroke-thickness stroke-thickness 
                             :on-click on-click :rounding rounding)))

(defun make-panel (&key (position (vec2 0 0)) (width 0) (height 0) children (fill-color gamekit.colors:+black+)
                     (stroke-color gamekit.colors:+transparent+) (stroke-thickness 0)
                     (rounding 0) image)
  "creates and returns a panel"
  (if image
      (make-instance 'image-panel :position position :image image :children children)
      (make-instance 'panel :position position :width width :height height :children children
                            :fill-color fill-color :stroke-color stroke-color
                            :stroke-thickness stroke-thickness :rounding rounding)))

(defun make-progress-bar (width height &key (position (vec2 0 0)) (fill-color gamekit.colors:+white+)
                                         (stroke-color gamekit.colors:+transparent+) (stroke-thickness 0)
                                         (inner-color gamekit.colors:+black+) (rounding 0) (percent 0)
                                         show-percent (font :default) (font-size 12) (font-color gamekit.colors:+black+))
  (let* ((inner-pos (vec2 (* .05 width) (* .2 height)))
         (inner-height (* .6 height))
         (inner-width (gamekit:lerp 0 (* .95 width) percent))
         (inner-panel (make-panel :position inner-pos :fill-color inner-color
                                  :width inner-width :height inner-height))
         (label (make-label (format nil "~A%" (round (* percent 100)))
                            :position (vec2 (- (/ width 2) 8)
                                            (+ (y inner-pos) 3))
                            :font font
                            :size font-size
                            :color font-color)))
    (make-instance 'progress-bar :position position :width width :height height :rounding rounding :percent percent
                                 :fill-color fill-color :stroke-color stroke-color :stroke-thickness stroke-thickness
                                 :label label :inner inner-panel :children (list inner-panel (when show-percent label)))))
