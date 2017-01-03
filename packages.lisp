(in-package :cl-user)

(defpackage :boomber.graphics
  (:use :cl)
  (:export :init-graphics
           :putpixel
           :rectangle
           :line
           :setfillstyle
           :floodfill
           :imagesize
           :getimage
           :putimage))

(defpackage :boomber.parser
  (:use :cl :yacc :cl-lex)
  (:export :defun-from-file))

(defpackage :boomber
  (:use :cl :boomber.parser :boomber.graphics)
  (:export :main
           :generate-texture-from-file))
