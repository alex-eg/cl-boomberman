(in-package :cl-user)

(defpackage :graphics
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

(defpackage :boomber
  (:use :cl :yacc :cl-lex :graphics)
  (:export :main
           :generate-texture-from-file))
