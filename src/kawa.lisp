;;; kawa.lisp -- Common Lisp / Kawa Scheme interop via OpenLDK
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:kawa)

;;; ---------------------------------------------------------------------------
;;; Runtime state
;;; ---------------------------------------------------------------------------

(defvar *kawa-initialized* nil
  "T after STARTUP has completed successfully.")

(defvar *scheme-instance* nil
  "The Kawa Scheme language singleton.")

(defvar *global-environment* nil
  "The global Kawa Scheme environment.")

;;; Cached static method symbols (resolved at startup)
(defvar *sym-scheme-getinstance* nil)
(defvar *sym-scheme-register-env* nil)
(defvar *sym-scheme-eval* nil)
(defvar *sym-scheme-get-std-env* nil)
(defvar *sym-language-set-defaults* nil)
(defvar *sym-symbol-valueof* nil)
(defvar *sym-intnum-make* nil)
(defvar *sym-dflonum-make* nil)

;;; Cached class symbols (resolved at startup)
(defvar *class-intnum* nil)
(defvar *class-dflonum* nil)
(defvar *class-string* nil)
(defvar *class-boolean* nil)
(defvar *class-pair* nil)
(defvar *class-llist* nil)
(defvar *class-integer* nil)
(defvar *class-long* nil)
(defvar *class-fstring* nil)
(defvar *class-istring* nil)

;;; Cached MethodHandles for procedure dispatch (resolved at startup)
(defvar *procedure-apply-to-object-mh* nil)
(defvar *procedure-apply-to-consumer-mh* nil)

(defun ensure-started ()
  "Signal an error if STARTUP has not been called."
  (unless *kawa-initialized*
    (error "cl-kawa: call (kawa:startup) first")))

;;; ---------------------------------------------------------------------------
;;; Symbol lookup helpers
;;; ---------------------------------------------------------------------------

(defun %find-static (name)
  "Find a static method symbol by searching OPENLDK.APP, OPENLDK.SYSTEM, :openldk."
  (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
        for pkg = (find-package pkg-name)
        when pkg
        do (let ((sym (find-symbol name pkg)))
             (when (and sym (fboundp sym))
               (return sym)))))

(defun %find-class (name)
  "Find a CLOS class symbol for a Java class by searching packages."
  (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
        for pkg = (find-package pkg-name)
        when pkg
        do (let ((sym (find-symbol name pkg)))
             (when (and sym (find-class sym nil))
               (return sym)))))

(defun %find-static-field (class-bin-name field-name)
  "Get the value of a static field on a Java class."
  (let ((static-name (format nil "+static-~A+" class-bin-name)))
    (loop for pkg-name in '("OPENLDK.APP" "OPENLDK.SYSTEM" "OPENLDK")
          for pkg = (find-package pkg-name)
          when pkg
          do (let ((sym (find-symbol static-name pkg)))
               (when (and sym (boundp sym))
                 (let ((static-obj (symbol-value sym)))
                   (return (slot-value static-obj (intern field-name :openldk)))))))))

;;; ---------------------------------------------------------------------------
;;; Value conversion: Scheme -> CL
;;; ---------------------------------------------------------------------------

(defun scheme->cl (obj)
  "Convert a Java/Kawa value to a Common Lisp value.
   Returns the object unchanged if no conversion applies."
  (cond
    ((null obj) nil)
    ((and *class-intnum* (typep obj *class-intnum*))
     (openldk::|longValue()| obj))
    ((and *class-dflonum* (typep obj *class-dflonum*))
     (openldk::|doubleValue()| obj))
    ((and *class-string* (typep obj *class-string*))
     (openldk::lstring obj))
    ((and *class-fstring* (typep obj *class-fstring*))
     (openldk::lstring (openldk::|toString()| obj)))
    ((and *class-istring* (typep obj *class-istring*))
     (openldk::lstring (openldk::|toString()| obj)))
    ((and *class-boolean* (typep obj *class-boolean*))
     (not (eql 0 (slot-value obj (intern "value" :openldk)))))
    ((and *class-pair* (typep obj *class-pair*))
     (cons (scheme->cl (openldk::|getCar()| obj))
           (scheme->cl (openldk::|getCdr()| obj))))
    ((and *class-llist* (typep obj *class-llist*))
     nil)
    ((and *class-integer* (typep obj *class-integer*))
     (slot-value obj (intern "value" :openldk)))
    ((and *class-long* (typep obj *class-long*))
     (slot-value obj (intern "value" :openldk)))
    (t obj)))

;;; ---------------------------------------------------------------------------
;;; Value conversion: CL -> Scheme
;;; ---------------------------------------------------------------------------

(defun cl->scheme (val)
  "Convert a Common Lisp value to a Java/Kawa object.
   Returns the value unchanged if no conversion applies."
  (cond
    ((null val) nil)
    ((integerp val)
     (cl:funcall *sym-intnum-make* val))
    ((floatp val)
     (cl:funcall *sym-dflonum-make* (coerce val 'double-float)))
    ((stringp val)
     (openldk::jstring val))
    ((consp val)
     (let ((pair (openldk::%make-java-instance "gnu/lists/Pair")))
       (openldk::|<init>(Ljava/lang/Object;Ljava/lang/Object;)| pair
        (cl->scheme (car val)) (cl->scheme (cdr val)))
       pair))
    (t val)))

;;; ---------------------------------------------------------------------------
;;; Startup
;;; ---------------------------------------------------------------------------

(defun startup (&key classpath)
  "Initialize the Kawa Scheme runtime.
   CLASSPATH is a string pointing to the Kawa JAR (colon-separated if multiple).
   If nil, reads from the LDK_CLASSPATH environment variable."
  (when *kawa-initialized*
    (return-from startup t))

  ;; Initialize the OpenLDK JVM runtime (loads JDK bootstrap classes)
  (openldk:initialize)

  ;; Set up boot and app class loaders (mirrors what openldk::main does).
  ;; initialize() loads JDK classes but doesn't create the LDK loader wrappers.
  (unless openldk::*boot-ldk-class-loader*
    (let ((system-pkg (or (find-package "OPENLDK.SYSTEM")
                          (make-package "OPENLDK.SYSTEM" :use '(:openldk)))))
      (setf openldk::*boot-ldk-class-loader*
            (make-instance 'openldk::<ldk-class-loader>
                           :id 0
                           :pkg system-pkg
                           :parent-loader nil
                           :java-loader nil
                           :ldk-classes-by-bin-name openldk::*ldk-classes-by-bin-name*
                           :ldk-classes-by-fq-name openldk::*ldk-classes-by-fq-name*
                           :java-classes-by-bin-name openldk::*java-classes-by-bin-name*
                           :java-classes-by-fq-name openldk::*java-classes-by-fq-name*))))
  (unless openldk::*app-ldk-class-loader*
    (setf openldk::*app-ldk-class-loader*
          (openldk::make-ldk-class-loader
           :parent-loader openldk::*boot-ldk-class-loader*
           :java-loader nil
           :package-name "OPENLDK.APP")))

  ;; Build the classpath: user JARs + JAVA_HOME jars
  (let* ((java-home (uiop:getenv "JAVA_HOME"))
         (java-jars (when java-home
                      (format nil "~{~A~^:~}"
                              (mapcar #'namestring
                                      (directory (concatenate 'string java-home "/lib/*.jar"))))))
         (kawa-cp (or classpath ""))
         (full-cp (if java-jars
                      (concatenate 'string kawa-cp ":" java-jars)
                      kawa-cp)))
    (setf openldk::*classpath*
          (loop for cpe in (split-sequence:split-sequence
                            (uiop:inter-directory-separator) full-cp)
                when (plusp (length cpe))
                collect (if (str:ends-with? ".jar" cpe)
                            (make-instance 'openldk::jar-classpath-entry :jarfile cpe)
                            (make-instance 'openldk::dir-classpath-entry :dir cpe)))))

  ;; Reset system properties and init Launcher (needed for MethodHandle infrastructure)
  (handler-case
      (let ((sym (%find-static "java/lang/System.initProperties(Ljava/util/Properties;)")))
        (when sym
          (cl:funcall sym
                      (slot-value openldk::|+static-java/lang/System+| (intern "props" :openldk)))))
    (error () nil))
  (handler-case
      (openldk::%clinit (openldk::%get-ldk-class-by-bin-name "sun/misc/Launcher" t))
    (error () nil))

  ;; Quiet down class loading output
  (setf openldk::*debug-load* nil)
  (setf openldk::*debug-compile* nil)

  ;; Load key Kawa classes
  (dolist (c '("kawa/standard/Scheme"
               "gnu/expr/Language"
               "gnu/mapping/Environment"
               "gnu/mapping/Symbol"
               "gnu/mapping/Procedure"
               "gnu/mapping/ProcedureN"
               "gnu/math/IntNum"
               "gnu/math/DFloNum"
               "gnu/lists/Pair"
               "gnu/lists/LList"
               "gnu/lists/FString"
               "gnu/lists/IString"))
    (handler-case (openldk::classload c)
      (error (e) (warn "cl-kawa: could not load ~A: ~A" c e))))

  ;; Resolve and cache static method symbols
  (setf *sym-scheme-getinstance*
        (%find-static "kawa/standard/Scheme.getInstance()"))
  (setf *sym-scheme-register-env*
        (%find-static "kawa/standard/Scheme.registerEnvironment()"))
  (setf *sym-scheme-eval*
        (%find-static "kawa/standard/Scheme.eval(Ljava/lang/String;Lgnu/mapping/Environment;)"))
  (setf *sym-scheme-get-std-env*
        (%find-static "kawa/standard/Scheme.getStdEnvironment()"))
  (setf *sym-language-set-defaults*
        (%find-static "gnu/expr/Language.setDefaults(Lgnu/expr/Language;)"))
  (setf *sym-symbol-valueof*
        (%find-static "gnu/mapping/Symbol.valueOf(Ljava/lang/String;)"))
  (setf *sym-intnum-make*
        (%find-static "gnu/math/IntNum.make(I)"))
  (setf *sym-dflonum-make*
        (%find-static "gnu/math/DFloNum.make(D)"))

  ;; Resolve and cache class symbols for type dispatch
  (setf *class-intnum*   (%find-class "gnu/math/IntNum"))
  (setf *class-dflonum*  (%find-class "gnu/math/DFloNum"))
  (setf *class-string*   (%find-class "java/lang/String"))
  (setf *class-boolean*  (%find-class "java/lang/Boolean"))
  (setf *class-pair*     (%find-class "gnu/lists/Pair"))
  (setf *class-llist*    (%find-class "gnu/lists/LList"))
  (setf *class-integer*  (%find-class "java/lang/Integer"))
  (setf *class-long*     (%find-class "java/lang/Long"))
  (setf *class-fstring*  (%find-class "gnu/lists/FString"))
  (setf *class-istring*  (%find-class "gnu/lists/IString"))

  ;; Get the Scheme singleton
  (setf *scheme-instance*
        (cl:funcall *sym-scheme-getinstance*))

  ;; Register standard environment and set Scheme as current language
  (cl:funcall *sym-scheme-register-env*)
  (cl:funcall *sym-language-set-defaults* *scheme-instance*)

  ;; Get the standard Scheme environment (with all builtins bound)
  (setf *global-environment*
        (cl:funcall *sym-scheme-get-std-env*))

  ;; Cache MethodHandles for cl-procedure dispatch.
  ;; Must be after Kawa init so <clinit> has run.
  (setf *procedure-apply-to-object-mh*
        (%find-static-field "gnu/mapping/ProcedureN" "applyToObject"))
  (setf *procedure-apply-to-consumer-mh*
        (%find-static-field "gnu/mapping/Procedure" "applyToConsumerDefault"))

  ;; Define the CL->Scheme procedure bridge class
  (%define-cl-procedure-class)

  (setf *kawa-initialized* t)
  t)

;;; ---------------------------------------------------------------------------
;;; S-expression serialization
;;; ---------------------------------------------------------------------------

(defun sexp->scheme-string (expr)
  "Convert a CL s-expression to a Scheme source string."
  (cond
    ((null expr) "'()")
    ((eq expr t) "#t")
    ((keywordp expr)
     (concatenate 'string "#:" (string-downcase (symbol-name expr))))
    ((symbolp expr)
     (let ((name (symbol-name expr)))
       (if (string= name (string-upcase name))
           (string-downcase name)          ; normal CL symbol: FOO -> foo
           name)))                         ; pipe-quoted: |toUpperCase| preserved
    ((stringp expr)
     (with-output-to-string (s)
       (write-char #\" s)
       (loop for ch across expr do
         (case ch
           (#\\ (write-string "\\\\" s))
           (#\" (write-string "\\\"" s))
           (t   (write-char ch s))))
       (write-char #\" s)))
    ((numberp expr)
     (let ((*read-default-float-format* 'double-float))
       (write-to-string expr)))
    ((and (consp expr) (eq (car expr) 'quote) (consp (cdr expr)) (null (cddr expr)))
     (concatenate 'string "'" (sexp->scheme-string (cadr expr))))
    ((consp expr)
     (with-output-to-string (s)
       (write-char #\( s)
       (loop for cell on expr
             for first = t then nil do
         (unless first (write-char #\Space s))
         (write-string (sexp->scheme-string (car cell)) s)
         (when (and (cdr cell) (not (consp (cdr cell))))
           (write-string " . " s)
           (write-string (sexp->scheme-string (cdr cell)) s)))
       (write-char #\) s)))
    ((characterp expr)
     (format nil "#\\~A" expr))
    ((vectorp expr)
     (with-output-to-string (s)
       (write-string "#(" s)
       (loop for i below (length expr)
             for first = t then nil do
         (unless first (write-char #\Space s))
         (write-string (sexp->scheme-string (aref expr i)) s))
       (write-char #\) s)))
    (t (write-to-string expr))))

;;; ---------------------------------------------------------------------------
;;; eval
;;; ---------------------------------------------------------------------------

(defun eval (expr &optional env)
  "Evaluate a Scheme expression.  EXPR can be a string of Scheme source
   or a CL s-expression (which is converted automatically).
   Returns the result as a CL value."
  (ensure-started)
  (let ((source (if (stringp expr) expr (sexp->scheme-string expr))))
    (scheme->cl
     (cl:funcall *sym-scheme-eval*
      (openldk::jstring source)
      (or env *global-environment*)))))

;;; ---------------------------------------------------------------------------
;;; lookup
;;; ---------------------------------------------------------------------------

(defun lookup (name &optional env)
  "Look up a Scheme binding by name.  Returns the raw Java object."
  (ensure-started)
  (let ((sym (cl:funcall *sym-symbol-valueof*
              (openldk::jstring name))))
    (openldk::|get(Lgnu/mapping/Symbol;)|
     (or env *global-environment*) sym)))

;;; ---------------------------------------------------------------------------
;;; funcall
;;; ---------------------------------------------------------------------------

(defun funcall (proc &rest args)
  "Call a Scheme procedure with CL arguments.  Returns the result as a CL value."
  (ensure-started)
  (let ((jargs (mapcar #'cl->scheme args)))
    (scheme->cl
     (case (length jargs)
       (0 (openldk::|apply0()| proc))
       (1 (openldk::|apply1(Ljava/lang/Object;)| proc (first jargs)))
       (2 (openldk::|apply2(Ljava/lang/Object;Ljava/lang/Object;)|
           proc (first jargs) (second jargs)))
       (3 (openldk::|apply3(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           proc (first jargs) (second jargs) (third jargs)))
       (4 (openldk::|apply4(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)|
           proc (first jargs) (second jargs) (third jargs) (fourth jargs)))
       (otherwise
        (openldk::|applyN([Ljava/lang/Object;)|
         proc
         (openldk::make-java-array
          :component-class "java/lang/Object"
          :initial-contents (coerce jargs 'vector))))))))

;;; ---------------------------------------------------------------------------
;;; register
;;; ---------------------------------------------------------------------------

(defvar *cl-procedure-class-defined* nil)

(defun %define-cl-procedure-class ()
  "Define the cl-procedure bridge class after Kawa classes are loaded."
  (when *cl-procedure-class-defined*
    (return-from %define-cl-procedure-class))

  (let ((proc-sym (%find-class "gnu/mapping/Procedure")))
    (unless proc-sym
      (warn "cl-kawa: gnu/mapping/Procedure class not found; register will not work")
      (return-from %define-cl-procedure-class))

    ;; Define the bridge class
    (cl:eval `(defclass kawa::cl-procedure (,proc-sym)
                ((kawa::%cl-fn :initarg :cl-function :accessor kawa::%cl-fn))))

    ;; 0-arg apply
    (cl:eval `(defmethod ,(intern "apply0()" :openldk) ((proc kawa::cl-procedure))
                (kawa:cl->scheme (cl:funcall (kawa::%cl-fn proc)))))

    ;; 1-arg apply
    (cl:eval `(defmethod ,(intern "apply1(Ljava/lang/Object;)" :openldk)
                  ((proc kawa::cl-procedure) arg)
                (kawa:cl->scheme (cl:funcall (kawa::%cl-fn proc) (kawa:scheme->cl arg)))))

    ;; 2-arg apply
    (cl:eval `(defmethod ,(intern "apply2(Ljava/lang/Object;Ljava/lang/Object;)" :openldk)
                  ((proc kawa::cl-procedure) a b)
                (kawa:cl->scheme (cl:funcall (kawa::%cl-fn proc)
                                             (kawa:scheme->cl a) (kawa:scheme->cl b)))))

    ;; N-arg apply
    (cl:eval `(defmethod ,(intern "applyN([Ljava/lang/Object;)" :openldk)
                  ((proc kawa::cl-procedure) args)
                (let ((cl-args (loop for i below (openldk::java-array-length args)
                                     collect (kawa:scheme->cl (openldk::jaref args i)))))
                  (kawa:cl->scheme (apply (kawa::%cl-fn proc) cl-args))))))

  (setf *cl-procedure-class-defined* t))

(defun register (name function &optional env)
  "Register a CL function as a Scheme procedure with the given NAME."
  (ensure-started)
  (let* ((proc (make-instance 'cl-procedure :cl-function function))
         (sym (cl:funcall *sym-symbol-valueof*
               (openldk::jstring name)))
         (jenv (or env *global-environment*)))
    ;; Set MethodHandle fields so Kawa's eval can call this procedure.
    ;; applyToObjectMethod -> ProcedureN.applyToObject: extracts args from
    ;; CallContext and dispatches to applyN (handled by our CLOS method).
    ;; applyToConsumerMethod -> Procedure.applyToConsumerDefault: wraps the
    ;; applyToObject call with consumer output handling.
    (when *procedure-apply-to-object-mh*
      (setf (slot-value proc (intern "applyToObjectMethod" :openldk))
            *procedure-apply-to-object-mh*))
    (when *procedure-apply-to-consumer-mh*
      (setf (slot-value proc (intern "applyToConsumerMethod" :openldk))
            *procedure-apply-to-consumer-mh*))
    (openldk::|define(Lgnu/mapping/Symbol;Ljava/lang/Object;Ljava/lang/Object;)| jenv sym nil proc)
    name))

;;; ---------------------------------------------------------------------------
;;; make-environment
;;; ---------------------------------------------------------------------------

(defun make-environment (&optional parent)
  "Create a new Scheme environment.  If PARENT is given, the new environment
   inherits bindings from it."
  (ensure-started)
  (let ((env (openldk::%make-java-instance "gnu/mapping/Environment")))
    (when parent
      (handler-case
          (openldk::|setParent(Lgnu/mapping/Environment;)| env parent)
        (error ()
          (warn "cl-kawa: could not set parent environment"))))
    env))
