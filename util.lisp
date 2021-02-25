(in-package :gamekit.ui)

(declaim (inline text-width text-height text-dimensions))

(defun find-or-create-font (font size)
  (unless (gethash font *font-cache*)
    (setf (gethash font *font-cache*) (make-hash-table)))

  (unless (gethash size (gethash font *font-cache*))
    (setf (gethash size (gethash font *font-cache*))
          (if (eql font :default)
              (bodge-canvas:make-default-font :size size)
              (gamekit:make-font font size))))

  (gethash size (gethash font *font-cache*)))

(defun text-width (text &key (font :default) (size 12))
  (let ((fnt (find-or-create-font font size)))
    (multiple-value-bind (_ text-width) (gamekit:calc-text-bounds text fnt)
      (declare (ignore _))
      text-width)))

(defun text-height (text &key (font :default) (size 12))
  (let ((fnt (find-or-create-font font size)))
    (multiple-value-bind (_ __ text-height) (gamekit:calc-text-bounds text fnt)
      (declare (ignore _ __))
      text-height)))

(defun text-dimensions (text &key (font :default) (size 12))
  (vec2 (text-width text :font font :size size)
        (text-height text :font font :size size)))
