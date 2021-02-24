(in-package :gamekit.ui)

(defun ui-mouse-handler (x y)
  (setf *mouse-position* (vec2 x y)))

(defun ui-click-handler ()
  (setf *mouse-clicked* t))

(defun ui-release-handler ()
  (setf *mouse-clicked* nil))
