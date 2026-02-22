;;; hello.lisp -- Hello World: Common Lisp -> Kawa Scheme -> Java
;;;
;;; Demonstrates the full interop chain:
;;;   Common Lisp calls Kawa Scheme, which calls a Java method,
;;;   and the result flows back to Common Lisp.
;;;
;;; Run:
;;;   LDK_CLASSPATH=libs/kawa-3.1.1.jar \
;;;     JAVA_HOME=/path/to/java8/jre \
;;;     sbcl --load hello.lisp

(asdf:load-system :cl-kawa)

(kawa:startup :classpath (or (uiop:getenv "LDK_CLASSPATH")
                             "libs/kawa-3.1.1.jar"))

;;; Common Lisp calls Kawa Scheme, Scheme calls Java's String.toUpperCase(),
;;; and the result flows back to Common Lisp.

(format t "~A~%"
  (kawa:eval '(let ((s (|java.lang.String| (string-append "Hello" ", " "World!"))))
	       (|s:toUpperCase|))))

(uiop:quit 0)
