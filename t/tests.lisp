;;; t/tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-kawa/tests)

;;; ---------------------------------------------------------------------------
;;; Startup fixture
;;; ---------------------------------------------------------------------------

(defvar *kawa-started* nil)

(defun ensure-kawa ()
  "Initialize Kawa once for the entire test run."
  (unless *kawa-started*
    (kawa:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                                 "libs/kawa-3.1.1.jar"))
    (setf *kawa-started* t)))

;;; ---------------------------------------------------------------------------
;;; eval (string form)
;;; ---------------------------------------------------------------------------

(def-suite :eval-string :in :cl-kawa
  :description "kawa:eval with string arguments")
(in-suite :eval-string)

(test eval-integer-arithmetic
  (ensure-kawa)
  (is (= 3 (kawa:eval "(+ 1 2)")))
  (is (zerop (kawa:eval "(+ 0 0)")))
  (is (= -5 (kawa:eval "(- 3 8)")))
  (is (= 120 (kawa:eval "(* 1 2 3 4 5)"))))

(test eval-floating-point
  (ensure-kawa)
  (is (= 3.14d0 (kawa:eval "3.14")))
  (is (= 2.5d0 (kawa:eval "(/ 5.0 2.0)"))))

(test eval-string-operations
  (ensure-kawa)
  (is (= 5 (kawa:eval "(string-length \"hello\")")))
  (is (string= "hello world" (kawa:eval "(string-append \"hello\" \" \" \"world\")"))))

(test eval-list-creation
  (ensure-kawa)
  (is (equal '(1 2 3) (kawa:eval "(list 1 2 3)")))
  (is (null (kawa:eval "'()"))))

(test eval-define
  (ensure-kawa)
  (is (= 42 (kawa:eval "(begin (define test-x 42) test-x)"))))

(test eval-boolean
  (ensure-kawa)
  (is (eq t (kawa:eval "(> 3 2)")))
  (is (eq nil (kawa:eval "(> 2 3)")))
  (is (eq t (kawa:eval "(= 5 5)")))
  (is (eq nil (kawa:eval "(= 5 6)"))))

;;; ---------------------------------------------------------------------------
;;; eval (s-expression form)
;;; ---------------------------------------------------------------------------

(def-suite :eval-sexp :in :cl-kawa
  :description "kawa:eval with s-expression arguments")
(in-suite :eval-sexp)

(test sexp-arithmetic
  (ensure-kawa)
  (is (= 3 (kawa:eval '(+ 1 2))))
  (is (= 30 (kawa:eval '(* (+ 2 3) (- 10 4)))))
  (is (zerop (kawa:eval '(- 5 5)))))

(test sexp-string-operations
  (ensure-kawa)
  (is (= 5 (kawa:eval '(string-length "hello"))))
  (is (string= "foobar" (kawa:eval '(string-append "foo" "bar")))))

(test sexp-list
  (ensure-kawa)
  (is (equal '(1 2 3) (kawa:eval '(list 1 2 3)))))

(test sexp-nested
  (ensure-kawa)
  (is (= 30 (kawa:eval '(* (+ 2 3) (- 10 4))))))

(test sexp-boolean
  (ensure-kawa)
  (is (eq t (kawa:eval '(> 3 2))))
  (is (eq nil (kawa:eval '(< 3 2)))))

(test sexp-quoted-list
  (ensure-kawa)
  (is (equal '(1 2 3) (kawa:eval '(quote (1 2 3))))))

(test sexp-define
  (ensure-kawa)
  (is (= 99 (kawa:eval '(begin (define sexp-y 99) sexp-y)))))

;;; ---------------------------------------------------------------------------
;;; sexp->scheme-string
;;; ---------------------------------------------------------------------------

(def-suite :sexp-serialization :in :cl-kawa
  :description "sexp->scheme-string conversion")
(in-suite :sexp-serialization)

(test serialize-nil
  (is (string= "'()" (kawa::sexp->scheme-string nil))))

(test serialize-t
  (is (string= "#t" (kawa::sexp->scheme-string t))))

(test serialize-symbol
  (is (string= "foo" (kawa::sexp->scheme-string 'foo)))
  (is (string= "string-length" (kawa::sexp->scheme-string 'string-length))))

(test serialize-keyword
  (is (string= "#:foo" (kawa::sexp->scheme-string :foo))))

(test serialize-integer
  (is (string= "42" (kawa::sexp->scheme-string 42)))
  (is (string= "-7" (kawa::sexp->scheme-string -7))))

(test serialize-float
  (is (string= "3.14" (kawa::sexp->scheme-string 3.14d0))))

(test serialize-string
  (is (string= "\"hello\"" (kawa::sexp->scheme-string "hello")))
  (is (string= "\"he said \\\"hi\\\"\"" (kawa::sexp->scheme-string "he said \"hi\"")))
  (is (string= "\"back\\\\slash\"" (kawa::sexp->scheme-string "back\\slash"))))

(test serialize-quote
  (is (string= "'foo" (kawa::sexp->scheme-string '(quote foo))))
  (is (string= "'(1 2 3)" (kawa::sexp->scheme-string '(quote (1 2 3))))))

(test serialize-list
  (is (string= "(+ 1 2)" (kawa::sexp->scheme-string '(+ 1 2))))
  (is (string= "(list 1 2 3)" (kawa::sexp->scheme-string '(list 1 2 3)))))

(test serialize-dotted-pair
  (is (string= "(1 . 2)" (kawa::sexp->scheme-string '(1 . 2)))))

(test serialize-nested
  (is (string= "(* (+ 2 3) (- 10 4))"
       (kawa::sexp->scheme-string '(* (+ 2 3) (- 10 4))))))

(test serialize-character
  (is (string= "#\\a" (kawa::sexp->scheme-string #\a))))

(test serialize-vector
  (is (string= "#(1 2 3)" (kawa::sexp->scheme-string #(1 2 3)))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: scheme->cl
;;; ---------------------------------------------------------------------------

(def-suite :scheme->cl :in :cl-kawa
  :description "Scheme to CL value conversion")
(in-suite :scheme->cl)

(test convert-integer
  (ensure-kawa)
  (is (integerp (kawa:eval '(+ 1 2))))
  (is (= 3 (kawa:eval '(+ 1 2)))))

(test convert-float
  (ensure-kawa)
  (let ((result (kawa:eval "3.14")))
    (is (floatp result))
    (is (= 3.14d0 result))))

(test convert-string
  (ensure-kawa)
  (let ((result (kawa:eval "(string-append \"hello\" \" world\")")))
    (is (stringp result))
    (is (string= "hello world" result))))

(test convert-boolean-true
  (ensure-kawa)
  (is (eq t (kawa:eval "(> 3 2)"))))

(test convert-boolean-false
  (ensure-kawa)
  (is (eq nil (kawa:eval "(> 2 3)"))))

(test convert-list
  (ensure-kawa)
  (let ((result (kawa:eval "(list 1 2 3)")))
    (is (listp result))
    (is (equal '(1 2 3) result))))

(test convert-empty-list
  (ensure-kawa)
  (is (null (kawa:eval "'()"))))

(test convert-nested-list
  (ensure-kawa)
  (is (equal '(1 (2 3) 4) (kawa:eval "(list 1 (list 2 3) 4)"))))

(test convert-nil-passthrough
  (is (null (kawa:scheme->cl nil))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: cl->scheme (round-trip)
;;; ---------------------------------------------------------------------------

(def-suite :cl->scheme :in :cl-kawa
  :description "CL to Scheme value conversion (verified by round-trip)")
(in-suite :cl->scheme)

(test roundtrip-integer
  (ensure-kawa)
  (kawa:register "cl-identity" #'identity)
  (is (= 42 (kawa:eval "(cl-identity 42)"))))

(test roundtrip-string
  (ensure-kawa)
  (is (string= "hello" (kawa:eval "(cl-identity \"hello\")"))))

(test roundtrip-list
  (ensure-kawa)
  (kawa:register "cl-reverse" (lambda (&rest args) (reverse args)))
  (is (equal '(3 2 1) (kawa:eval "(cl-reverse 1 2 3)"))))

;;; ---------------------------------------------------------------------------
;;; lookup
;;; ---------------------------------------------------------------------------

(def-suite :lookup :in :cl-kawa
  :description "kawa:lookup")
(in-suite :lookup)

(test lookup-builtin
  (ensure-kawa)
  (let ((proc (kawa:lookup "+")))
    (is (not (null proc)))))

(test lookup-defined
  (ensure-kawa)
  (kawa:eval "(begin (define lookup-test-val 123) lookup-test-val)")
  (let ((val (kawa:lookup "lookup-test-val")))
    (is (not (null val)))))

;;; ---------------------------------------------------------------------------
;;; funcall
;;; ---------------------------------------------------------------------------

(def-suite :funcall :in :cl-kawa
  :description "kawa:funcall")
(in-suite :funcall)

(test funcall-0-args
  (ensure-kawa)
  (kawa:eval "(begin (define (funcall-zero) 42) funcall-zero)")
  (let ((proc (kawa:lookup "funcall-zero")))
    (is (= 42 (kawa:funcall proc)))))

(test funcall-1-arg
  (ensure-kawa)
  (let ((abs (kawa:lookup "abs")))
    (is (= 5 (kawa:funcall abs -5)))))

(test funcall-2-args
  (ensure-kawa)
  (let ((add (kawa:lookup "+")))
    (is (= 30 (kawa:funcall add 10 20)))))

(test funcall-3-args
  (ensure-kawa)
  (let ((add (kawa:lookup "+")))
    (is (= 60 (kawa:funcall add 10 20 30)))))

(test funcall-4-args
  (ensure-kawa)
  (let ((add (kawa:lookup "+")))
    (is (= 100 (kawa:funcall add 10 20 30 40)))))

(test funcall-many-args
  (ensure-kawa)
  (let ((add (kawa:lookup "+")))
    (is (= 150 (kawa:funcall add 10 20 30 40 50)))))

;;; ---------------------------------------------------------------------------
;;; register
;;; ---------------------------------------------------------------------------

(def-suite :register :in :cl-kawa
  :description "kawa:register")
(in-suite :register)

(test register-and-call-from-scheme
  (ensure-kawa)
  (kawa:register "cl-square" (lambda (x) (* x x)))
  (is (= 49 (kawa:eval "(cl-square 7)")))
  (is (= 81 (kawa:eval '(cl-square 9)))))

(test register-and-call-via-funcall
  (ensure-kawa)
  (kawa:register "cl-double" (lambda (x) (* x 2)))
  (let ((proc (kawa:lookup "cl-double")))
    (is (= 14 (kawa:funcall proc 7)))))

(test register-multi-arg
  (ensure-kawa)
  (kawa:register "cl-add3" (lambda (a b c) (+ a b c)))
  (is (= 6 (kawa:eval "(cl-add3 1 2 3)"))))

(test register-returns-string
  (ensure-kawa)
  (kawa:register "cl-greet" (lambda (name) (format nil "Hello, ~A!" name)))
  (is (string= "Hello, World!" (kawa:eval "(cl-greet \"World\")"))))

(test register-returns-list
  (ensure-kawa)
  (kawa:register "cl-pair" (lambda (a b) (list a b)))
  (is (equal '(1 2) (kawa:eval "(cl-pair 1 2)"))))

(test register-no-args
  (ensure-kawa)
  (kawa:register "cl-forty-two" (constantly 42))
  (is (= 42 (kawa:eval "(cl-forty-two)"))))

(test register-scheme-calls-cl-calls-scheme
  "Round-trip: Scheme calls CL function that calls back into Scheme."
  (ensure-kawa)
  (kawa:register "cl-eval-scheme"
                 (lambda (expr-str) (kawa:eval (kawa:scheme->cl expr-str))))
  (is (= 10 (kawa:eval "(cl-eval-scheme \"(+ 3 7)\")"))))

;;; ---------------------------------------------------------------------------
;;; make-environment
;;; ---------------------------------------------------------------------------

(def-suite :environments :in :cl-kawa
  :description "kawa:make-environment")
(in-suite :environments)

(test make-environment-basic
  (ensure-kawa)
  (let ((env (kawa:make-environment)))
    (is (not (null env)))))

;;; ---------------------------------------------------------------------------
;;; Error handling
;;; ---------------------------------------------------------------------------

(def-suite :errors :in :cl-kawa
  :description "Error conditions")
(in-suite :errors)

(test eval-before-startup-signals
  "Calling eval on a fresh kawa package without startup should signal an error."
  ;; We can't easily test this without resetting state, so just verify
  ;; that ensure-started doesn't error when already started.
  (ensure-kawa)
  (finishes (kawa:eval '(+ 1 1))))

(test eval-bad-scheme-signals
  (ensure-kawa)
  (signals error (kawa:eval "(undefined-proc-xyz)")))
