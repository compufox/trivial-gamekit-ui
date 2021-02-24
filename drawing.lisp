(in-package :gamekit.ui)

(defmethod draw-widget ((this widget))
  (typecase this
    (label (draw-label this))
    (panel (draw-panel this))
    (button (draw-button this))))

(defmethod draw-label ((this label) &key (offset (vec2 0 0)))
  (unless (and (width this) (height this))
    (update-bounds this))
  (gamekit:draw-text (text this) (gamekit:add offset (widget-position this))
                     :fill-color (text-color this)
                     :font (find-or-create-font (font this) (size this))))

(defmethod draw-button ((this button) &key (offset (vec2 0 0)))
  (with-slots (label rounding width height position
               pressed-color hover-color fill-color
               stroke-color stroke-thickness pressed
               on-click) this
    (unless (and (width this) (height this)
                 (width label) (height label))
      (update-bounds this))
    (let ((col (cond ((and (mouse-in-button this :offset offset) *mouse-clicked*)
                      (progn
                        (setf pressed t)
                        pressed-color))
                     ((mouse-in-button this :offset offset)
                      (progn
                        (when (and pressed on-click)
                          (setf pressed nil)
                          (funcall on-click)
                          (update-bounds this))
                        hover-color))
                     (t (progn (setf pressed nil) fill-color)))))
      (gamekit:draw-rect (gamekit:add offset position)
                         width height
                         :rounding rounding
                         :fill-paint col
                         :stroke-paint stroke-color
                         :thickness stroke-thickness)
      (draw-label label :offset (gamekit:add (gamekit:add offset position)
                                             (vec2 (* .13 (width label))
                                                   (* .3 (height label))))))))

(defmethod draw-button ((this image-button) &key (offset (vec2 0 0)))
  (with-slots (pressed-image pressed
               hover-image image on-click) this
    (unless (width this)
      (setf (width this) (gamekit:image-width image)
            (height this) (gamekit:image-height image)))
    
    (let ((im (cond ((and (mouse-in-button this :offset offset) *mouse-clicked*)
                     (progn
                       (setf pressed t)
                       pressed-image))
                    ((mouse-in-button this :offset offset)
                     (when (and pressed on-click)
                       (setf pressed nil)
                       (funcall on-click)
                       (update-bounds this))
                     hover-image)
                    (t (progn (setf pressed nil) image)))))
      (gamekit:draw-image (gamekit:add offset (widget-position this)) im))))

(defmethod draw-panel ((this panel) &key (offset (vec2 0 0)))
  (with-slots (children position width height fill-color rounding) this
    (gamekit:draw-rect (gamekit:add offset position) width height
                       :fill-paint fill-color
                       :rounding rounding)
    (loop for child in children
          do (typecase child
               (label (draw-label child :offset (gamekit:add offset position)))
               (button (draw-button child :offset (gamekit:add offset position)))
               (panel (draw-panel child :offset (gamekit:add offset position)))))))

(defmethod draw-panel ((this image-panel) &key (offset (Vec2 0 0)))
  (with-slots (children position image) this
    (gamekit:draw-image (gamekit:add position offset) image)
    (loop for child in children
          do (typecase child
               (label (draw-label child :offset (gamekit:add offset position)))
               (button (draw-button child :offset (gamekit:add offset position)))
               (panel (draw-panel child :offset (gamekit:add offset position)))))))


(defmethod mouse-in-button ((this button) &key (offset (vec2 0 0)))
  (with-slots (width height) this
    (let ((position (gamekit:add offset (slot-value this 'position))))
      (and (<= (x position) (x *mouse-position*))
           (<= (x *mouse-position*) (+ (x position) width))
           
           (<= (y position) (y *mouse-position*))
           (<= (y *mouse-position*) (+ (y position) height))))))
         
