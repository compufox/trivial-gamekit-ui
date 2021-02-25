(in-package :gamekit.ui)

(defclass widget ()
  ((width :initarg :width
          :initform nil
          :accessor width)
   (height :initarg :height
           :initform nil
           :accessor height)
   (position :initarg :position
             :initform (vec2 0 0)
             :accessor widget-position)))

(defclass with-text ()
  ((text :initarg :text
         :initform ""
         :accessor text)
   (font :initarg :font
         :initform nil
         :accessor font)
   (size :initarg :size
         :initform 12
         :accessor size)
   (text-color :initarg :text-color
               :initform gamekit.colors:+black+
               :accessor text-color)))

(defclass with-image ()
  ((image :initarg :image
          :initform nil
          :accessor image)
   (pressed-image :initarg :pressed-image
                  :initform nil
                  :accessor pressed-image)
   (hover-image :initarg :hover-image
                :initform nil
                :accessor hover-image)))

(defclass with-stroke ()
  ((rounding :initarg :rounding
             :initform 0
             :accessor rounding)
   (stroke-color :initarg :stroke-color
                 :initform gamekit.colors:+transparent+
                 :accessor stroke-color)
   (stroke-thickness :initarg :stroke-thickness
                     :initform 1
                     :accessor stroke-thickness)))

(defclass label (widget with-text) ())

(defclass button (widget with-stroke)
  ((label :initarg :label
          :initform nil
          :accessor label)
   (fill-color :initarg :fill-color
               :initform gamekit.colors:+red+
               :accessor fill-color)
   (hover-color :initarg :hover-color
                :initform gamekit.colors:+red+
                :accessor hover-color)
   (pressed-color :initarg :pressed-color
                  :initform gamekit.colors:+red+
                  :accessor pressed-color)
   (pressed :initform nil)
   (on-click :initarg :on-click
             :initform nil
             :accessor on-click)))

(defclass image-button (button with-image) ())

(defclass panel (widget with-stroke)
  ((children :initarg :children
             :initform nil
             :accessor children)
   (fill-color :initarg :fill-color
               :initform gamekit.colors:+black+
               :accessor fill-color)))

(defclass image-panel (panel with-image) ())
 

(defmethod (setf size) :after (value (this label))
  (setf (width this) nil
        (height this) nil))

(defmethod (setf text) :after (value (this label))
  (setf (width this) nil
        (height this) nil))

(defmethod (setf text) (value (this button))
  (setf (text (label this)) value))

(defmethod update-bounds ((this label))
  (let ((dims (text-dimensions (text this) :font (font this) :size (size this))))
    (setf (width this) (x dims)
          (height this) (y dims))))

(defmethod update-bounds ((this button))
  (with-slots (label) this
    (update-bounds label)
    (setf (width this) (* 1.25 (width label))
          (height this) (* 1.25 (height label)))))
