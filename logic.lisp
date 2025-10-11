;; logic.lisp

(in-package :logic)

(defun condition-met-p (state test)
  "Ensure that a given condition is met"
  (funcall test state))
