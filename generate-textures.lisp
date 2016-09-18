(cl:defpackage :texture-loader
  (:use :cl :yacc :cl-lex)
  (:export :generate-texture-from-file))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload :yacc)
  (ql:quickload :alexandria)
  (ql:quickload :cl-lex))

(in-package :texture-loader)

(define-string-lexer graphics-h-lexer
  ("//.*")                 ; comment
  ("[0-9][0-9]*" (return (values 'int (read-from-string $@))))
  ("for" (return 'for))
  ("[a-zA-Z]([a-zA-Z]|[0-9]|_)*" (return (values 'id (read-from-string $@))))
  ("\\+\\+" (return (values '++ '++)))
  ("--" (return (values '-- '--)))
  ("\\+" (return (values '+ '+)))
  ("}" (return '}))
  ("{" (return '{))
  (";" (return 'SC))
  ("=" (return '=))
  ("\\(" (return 'LP))
  ("\\)" (return 'RP))
  ("<" (return (values '< '<)))
  ("," (return 'CO)))

(define-parser *boomber-parser*
  (:start-symbol program)
  (:terminals
   (++ -- } { SC = LP RP < + CO id int
       for))

  ;; Expressions

  (expr
   assignment-expr
   (expr CO assignment-expr))

  (assignment-expr
   relational-expr
   (id id = assignment-expr)
   (relational-expr = assignment-expr))

  (relational-expr
   additive-expr
   (relational-expr < additive-expr))

  (additive-expr
   unary-expr
   (additive-expr + unary-expr))

  (unary-expr
   postfix-expr
   (++ unary-expr)
   (-- unary-expr))

  (postfix-expr
   primary-expr
   (postfix-expr LP RP (lambda (expr lp rp) (function-call expr lp nil rp)))
   (postfix-expr LP arg-list RP)
   (postfix-expr -- (lambda (expr op) (unary-op op expr)))
   (postfix-expr ++ (lambda (expr op) (unary-op op expr))))

  (primary-expr
   id
   int
   (LP expr RP))

  (arg-list
   assignment-expr
   (arg-list CO assignment-expr))

  ;; Statements

  (st
   expr-st
   compound-st
   for-st)

  (expr-st
   SC
   (expr SC))

  (compound-st
   ({ } (lambda ({ }) (compound-statement { nil })))
   ({ st-list }))

  (st-list
   st
   (st-list st))

  (for-st
   (for LP expr-st expr-st RP st)
   (for LP expr-st expr-st expr RP st))

  ;; Entry

  (program
   st
   (program st)))

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

(defun strings+ (strings)
  (apply #'concatenate (cons 'string strings)))

(defun fix-newlines (string)
  (map 'string (lambda (c)
                 (if (char= c #\Return)
                     #\Newline
                     c))
       string))

(defun generate-texture-from-file (file-path)
  (let ((strings (fix-newlines
                  (strings+
                   (read-texture-file file-path)))))
    strings))
