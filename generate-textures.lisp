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
  (">" (return (values '> '>)))
  ("," (return 'CO)))

(eval-when (:compile-toplevel)
  (defun regular-assignment (expr-1 _op expr-2)
    (declare (ignore _op))
    (list 'defparameter expr-1 expr-2))

  (defun typed-assignment (_type id _op expr)
    (declare (ignore _type))
    (regular-assignment id _op expr))

  (defun infix-to-prefix (expr-1 op expr-2)
    (list op expr-1 expr-2))

  (defun progn-exprs (expr-1 _co expr-2)
    (declare (ignore _co))
    (if (eql 'progn (car expr-1))
        (append expr-1 (list expr-2))
        (list 'progn expr-1 expr-2)))

  (defun unary-op (op expr)
    (cond ((eql op '++) (list 'incf expr))
          ((eql op '--) (list 'decf expr))))

  (defun function-call (expr _lp args _rp)
    (declare (ignore _lp _rp))
    (if (listp args)
        (cons expr
              (nreverse (alexandria:flatten args)))
        (cons expr (list args))))

  (defun paren-expr (_lp expr _rp)
    (declare (ignore _lp _rp))
    expr)

  (defun arg-list (car _co cdr)
    (declare (ignore _co))
    (cons cdr (list car)))

  (defun drop-semicolon (expr _semicolon)
    (declare (ignore _semicolon))
    expr)

  (defun compound-statement ({ statement-list })
    (declare (ignore { }))
    statement-list)

  (defun statement-list (list statement)
    (if (eql 'progn (car list))
        (append list (list statement))
        (append (list 'progn list) (list statement))))

  (defun for-statement (_for _lp init-list condition-list step-list _rp body)
    (declare (ignore _for _lp _rp))
    (let (conditions)
      (if (eql 'progn (car condition-list))
          (dolist (c (cdr condition-list) (push 'progn conditions))
            (push `(unless ,c (go exit)) conditions))
          (setf conditions `(unless ,condition-list (go exit))))
      `(progn
         ,init-list
         (tagbody
          start
            ,conditions
            ,body
            ,step-list
            (go start)
          exit (values)))))

  (defun program (program st)
    (if (eql 'progn (car program))
        (append program (list st))
        (list 'progn program st))))


(define-parser *boomber-parser*
  (:start-symbol program)
  (:terminals
   (++ -- } { SC = LP RP < > + CO id int
       for))

  ;; Expressions

  (expr
   assignment-expr
   (expr CO assignment-expr #'progn-exprs))

  (assignment-expr
   relational-expr
   (id id = assignment-expr #'typed-assignment)
   (relational-expr = assignment-expr #'regular-assignment))

  (relational-expr
   additive-expr
   (relational-expr < additive-expr #'infix-to-prefix)
   (relational-expr > additive-expr #'infix-to-prefix))

  (additive-expr
   unary-expr
   (additive-expr + unary-expr #'infix-to-prefix))

  (unary-expr
   postfix-expr
   (++ unary-expr #'unary-op)
   (-- unary-expr #'unary-op))

  (postfix-expr
   primary-expr
   (postfix-expr LP RP (lambda (expr lp rp) (function-call expr lp nil rp)))
   (postfix-expr LP arg-list RP #'function-call)
   (postfix-expr -- (lambda (expr op) (unary-op op expr)))
   (postfix-expr ++ (lambda (expr op) (unary-op op expr))))

  (primary-expr
   id
   int
   (LP expr RP #'paren-expr))

  (arg-list
   assignment-expr
   (arg-list CO assignment-expr #'arg-list))

  ;; Statements

  (st
   expr-st
   compound-st
   for-st)

  (expr-st
   SC
   (expr SC #'drop-semicolon))

  (compound-st
   ({ } (lambda ({ }) (compound-statement { nil })))
   ({ st-list } #'compound-statement))

  (st-list
   st
   (st-list st #'statement-list))

  (for-st
   (for LP expr-st expr-st RP st)
   (for LP expr-st expr-st expr RP st #'for-statement))

  ;; Entry

  (program
   st
   (program st #'program)))

(defun read-texture-file (file-path)
  (with-open-file (s file-path)
    (loop :for l := nil :then
          (read-line s nil :EOF)
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
    (parse-with-lexer (graphics-h-lexer strings) *boomber-parser*)))
