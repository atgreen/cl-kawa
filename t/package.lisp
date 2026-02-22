;;; t/package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cl-kawa/tests
  (:use #:cl #:fiveam)
  (:documentation "Tests for cl-kawa."))

(in-package #:cl-kawa/tests)

(def-suite :cl-kawa
  :description "cl-kawa test suite")

(in-suite :cl-kawa)
