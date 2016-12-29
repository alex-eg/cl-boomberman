(in-package :boomber)

(defmacro switch-keysym ((keysym-var) &body body)
  (let ((processed
         (mapcar
          (lambda (e)
            (let ((keysym (car e))
                  (progn-body (cdr e)))
              `((sdl2:scancode= (sdl2:scancode-value ,keysym-var)
                                ,keysym)
                (progn ,@progn-body))))
          body)))
    `(cond ,@processed)))

(defvar *width* 640)
(defvar *height* 480)
(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Boomberman-cl BITCH"
                           :w *width*
                           :h *height*)
      (sdl2:with-renderer (ren win :flags '(:accelerated))
        (let ((x 0)
              (y 0))
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (switch-keysym (keysym)
               (:scancode-escape
                (sdl2:push-event :quit))
               (:scancode-up
                (setf y (- y 20)))
               (:scancode-down
                (setf y (+ y 20)))
               (:scancode-right
                (setf x (+ x 20)))
               (:scancode-left
                (setf x (- x 20)))))

            (:quit () t)

            (:idle
             ()
             (sdl2:delay 100)
             (sdl2:render-clear ren)
             (sdl2::%with-rect ((r x y 20 20))
               (sdl2:render-draw-rect ren
                                      r))
             (sdl2:render-present ren))))))))
