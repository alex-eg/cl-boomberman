(in-package :cl-user)

(asdf:defsystem :boomber
    :depends-on (:sdl2 :yacc :cl-lex
                       :alexandria)
    :components ((:file "packages")
                 (:file "main")
                 (:file "generate-textures")
                 (:file "graphics")))
