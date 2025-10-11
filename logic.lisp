;; logic.lisp

(defpackage #:logic
  (:use :cl :cl-user)
  (:export condition-met-p))

(in-package :logic)

(defun condition-met-p (state test)
  "Ensure that a given condition is met"
  (funcall test state))
