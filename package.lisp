;;;; package.lisp

(defpackage #:logic
  (:use #:cl)
  (:export condition-met-p))

(defpackage #:map-tools
  (:use #:cl)
  (:export game-map room extend-map get-room gen-connection))

(defpackage #:entity
  (:use #:cl)
  (:export +hostility+ +senses +races+ person object))

(defpackage #:event
  (:use #:cl #:logic #:map-tools #:entity)
  (:export make-context
           +feelings+ +status-effects+ +apparitions+ +event-classes+ events
           simulate-events simulate-conditions evaluate-probability simulate-event
           simulate-effect simulate-effects simulate-event))

(defpackage #:parse
  (:use #:cl #:map-tools #:event #:entity)
  (:export read-commands))

(defpackage #:stork
  (:use #:cl #:parse #:event))
