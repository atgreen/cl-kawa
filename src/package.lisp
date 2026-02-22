;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:kawa
  (:use #:cl)
  (:nicknames #:cl-kawa)
  (:shadow #:eval #:funcall)
  (:documentation "Common Lisp / Kawa Scheme interop via OpenLDK.")
  (:export #:startup
           #:eval
           #:funcall
           #:lookup
           #:register
           #:scheme->cl
           #:cl->scheme
           #:make-environment))

(in-package #:kawa)
