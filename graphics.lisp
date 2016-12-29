(in-package :graphics)

(defmacro color-rgba (r g b a)
  (logior (ash r 24)
          (ash g 16)
          (ash b 8)
          a))

(defun r (int-32)
  (ash int-32 -24))

(defun g (int-32)
  (ash
   (logand int-32 (ash 255 16))
   -16))

(defun b (int-32)
  (ash
   (logand int-32 (ash 255 8))
   -8))

(defun a (int-32)
  (logand int-32 255))

(defparameter +color-list+
  (list ('black . (color-rgba 0 0 0 0))
        ('blue . (color-rgba 0 0 170 0))
        ('green . (color-rgba 0 170 0 0))
        ('cyan . (color-rgba 0 170 170 0))
        ('red . (color-rgba 170 0 0 0))
        ('magenta . (color-rgba 170 0 170 0))
        ('brown . (color-rgba 170 85 0 0))
        ('lightgray . (color-rgba 170 170 170 0))
        ('darkgray . (color-rgba 85 85 85 0))
        ('lightblue . (color-rgba 85 85 255 0))
        ('lightgreen . (color-rgba 85 255 85 0))
        ('lightcyan . (color-rgba 85 255 255 0))
        ('lightred . (color-rgba 255 85 85 0))
        ('lightmagenta . (color-rgba 255 85 255 0))
        ('yellow . (color-rgba 255 255 85 0))
        ('white . (color-rgba 255 255 255 0))))

(defun color (num-or-name)
  "Gets color from list by name or position"
  (cdr (if (symbolp num-or-name)
           (assoc num-or-name +color-list+)
           (nth num-or-name +color-list+))))

(defvar *surface* nil
  "Current drawing surface")

(defun rectangle (x y w h))
(defun line (x0 y0 x1 y1))
(defun putpixel (x y color))

(defun setfillstyle (style color))
(defun floodfill (x y color))

(defun imagesize (x y w h))
(defun getimage (x y w h ptr))
(defun putimage (x y ptr mode))
