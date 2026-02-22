;;; test.lisp -- cl-kawa smoke tests
;;;
;;; Run: LDK_CLASSPATH=libs/kawa-3.1.1.jar JAVA_HOME=... sbcl --load test.lisp

(require :asdf)

;; Register both cl-kawa and openldk so ASDF can find them
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)
(push #p"~/git/openldk/"
      asdf:*central-registry*)

(asdf:load-system :cl-kawa)

(format t "~%=== cl-kawa tests ===~%")

(kawa:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                             "libs/kawa-3.1.1.jar"))

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro test (name expr expected &key (test '#'equal))
  `(handler-case
       (let ((result ,expr))
         (if (funcall ,test result ,expected)
             (progn (format t "  PASS: ~A~%" ,name)
                    (incf *pass*))
             (progn (format t "  FAIL: ~A~%    expected: ~S~%    got:      ~S~%"
                            ,name ,expected result)
                    (incf *fail*))))
     (error (e)
       (format t "  ERROR: ~A~%    ~A~%" ,name e)
       (incf *fail*))))

;; Basic eval
(test "integer arithmetic" (kawa:eval "(+ 1 2)") 3)
(test "string-length" (kawa:eval "(string-length \"hello\")") 5)
(test "list creation" (kawa:eval "(list 1 2 3)") '(1 2 3))
(test "define value" (kawa:eval "(begin (define x 42) x)") 42)
(test "boolean true" (kawa:eval "(> 3 2)") t)
(test "boolean false" (kawa:eval "(> 2 3)") nil)

;; Lookup + funcall
(test "lookup + funcall"
      (let ((add (kawa:lookup "+")))
        (kawa:funcall add 10 20))
      30)

;; Register CL function
(test "register + call"
      (progn
        (kawa:register "cl-square" (lambda (x) (* x x)))
        (kawa:eval "(cl-square 7)"))
      49)

;; S-expression eval (no strings needed)
(test "sexp arithmetic" (kawa:eval '(+ 1 2)) 3)
(test "sexp string-length" (kawa:eval '(string-length "hello")) 5)
(test "sexp list" (kawa:eval '(list 1 2 3)) '(1 2 3))
(test "sexp nested" (kawa:eval '(* (+ 2 3) (- 10 4))) 30)
(test "sexp boolean" (kawa:eval '(> 3 2)) t)
(test "sexp quoted list" (kawa:eval '(quote (1 2 3))) '(1 2 3))
(test "sexp define + call"
      (kawa:eval '(begin (define y 99) y))
      99)
(test "sexp register call"
      (kawa:eval '(cl-square 9))
      81)

(format t "~%~D passed, ~D failed.~%" *pass* *fail*)
(uiop:quit (if (zerop *fail*) 0 1))
