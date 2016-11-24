(in-package :cl-user)

(asdf:defsystem :boomber
    :depends-on (:sdl2 :yacc :cl-lex
                       :alexandria)
    :components ((:file "main")
                 (:file "generate-textures")))
