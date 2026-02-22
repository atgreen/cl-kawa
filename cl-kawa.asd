;;; cl-kawa.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(asdf:defsystem "cl-kawa"
  :description "Common Lisp / Kawa Scheme interop via OpenLDK."
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     (:read-file-form "version.sexp")
  :depends-on  ("openldk")
  :serial t
  :components ((:file "src/package")
               (:file "src/kawa"))
  :in-order-to ((test-op (test-op "cl-kawa/tests"))))

(asdf:defsystem "cl-kawa/tests"
  :description "Tests for cl-kawa."
  :depends-on  ("cl-kawa" "fiveam")
  :serial t
  :components ((:file "t/package")
               (:file "t/tests"))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! :cl-kawa)))
