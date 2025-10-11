;;;; package.lisp

(defpackage #:stork
  (:use #:cl))

(defpackage #:logic
  (:use #:cl)
  (:export condition-met-p))

(defpackage #:event
  (:use #:cl #:logic #:map #:character)
  (:export context +feelings+ +status-effects+ +apparitions+ +event-classes+ events
           simulate-events simulate-conditions evaluate-probability simulate-event
           simulate-effect simulate-effects simulate-event))

(defpackage #:map
  (:use #:cl)
  (:export game-map room extend-map get-room gen-connection))

(defpackage #:entity
  (:use #:cl)
  (:export +hostility+ +senses +races+ character object))
