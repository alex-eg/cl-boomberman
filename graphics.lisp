(in-package :boomber)

(defvar *color* 'black
  "Current outline color")

(defvar *fill-color* 'black
  "Current outline color")

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
