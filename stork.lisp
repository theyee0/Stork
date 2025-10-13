;;;; stork.lisp

(in-package #:stork)

(defparameter state (event:make-context))

(defun main ()
  (setf (event::context-current-room state) (map-tools::make-map-room))
  (setf (event::context-tick state) (+ (random 1000) 7))

  (format t ">")
  (loop :while (parse:read-commands state (read-line))
        :do
           (event:simulate-events state event:events)
           (format t ">")))
