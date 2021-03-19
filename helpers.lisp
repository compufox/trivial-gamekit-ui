(in-package :gamekit.ui)

(defun ui-mouse-handler (x y)
  (setf *mouse-position* (vec2 x y)))

(defun ui-click-handler ()
  (setf *mouse-clicked* t))

(defun ui-release-handler ()
  (setf *mouse-clicked* nil))


;; gamekit class/state stuff
(defclass with-ui ()
  ((ui :initform (make-hash-table)))
  (:documentation "represents trivial gamekit game/state that has a UI"))

(defmethod ui-element ((this with-ui) elt)
  "get a widget from THIS identified by ELT"
  (gethash elt (slot-value this 'ui)))

(defmethod (setf ui-element) (value (this with-ui) elt)
  (setf (gethash elt (slot-value this 'ui)) value))

(defmethod draw-ui ((this with-ui))
  "iterate over all ui-elements of THIS and draw them"
  (loop :for w :being :the :hash-value :of (slot-value this 'ui)
        :do (draw-widget w)))

(defmacro initialize-ui (state &rest forms)
  "macro that allows you to set multiple widgets in STATE-OR-GAME at once

each form in FORMS should be (IDENTIFIER WIDGET)"
  `(progn
     ,@(loop :for (k . v) :in forms
             :collect `(setf (ui-element ,state ,k) ,@v))))
