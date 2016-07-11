(defun read-texture-file (file-path)
  (with-open-file (s file-path)
    (loop :for l := nil :then
       (handler-bind
           ((sb-int:stream-decoding-error
             (lambda (c)
               (when (find-restart 'sb-impl::input-replacement c)
                 (invoke-restart 'sb-impl::input-replacement
                                 "?")))))
         (read-line s nil :EOF))
       :until (eq l :EOF)
       :collect l)))

(defun parse-putpixel (putpixel-string)
  (cl-ppcre:register-groups-bind (x y c)
      ("\\((.*),(.*),(.*)\\)" putpixel-string)
    (list 'PUT
          (read-from-string x)
          (read-from-string y)
          (read-from-string c))))

(defun parse-for (for-string)
  (cl-ppcre:register-groups-bind (init cond step)
      ("\\((.*);(.*);(.*)\\)" for-string) (list init cond step)))

(defun tokenize (texture-string-list)
  (reverse
   (reduce (lambda (a l)
             (cond
               ((cl-ppcre:scan ".*putpixel" l)
                (cons (parse-putpixel l) a))
               ((cl-ppcre:scan ".*for\\(" l)
                (cons (parse-for l) a))
               ((cl-ppcre:scan ".*}" l)
                (cons 'ENDFOR a))
               (t a)))
           texture-string-list
           :initial-value nil)))
