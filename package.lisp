;;;; package.lisp

(defpackage :logic
  (:use :cl)
  (:export condition-met-p))

(defpackage :entity
  (:use :cl)
  (:export +hostility+ +senses +races+ make-person make-object
           move attack search tiptoe run look use wait))

(defpackage :event
  (:use :cl)
  (:export make-context context-self context-player context-game-map context-current-room
           context-status context-objects context-entities
           +feelings+ +status-effects+ +apparitions+ +event-classes+ events
           simulate-events simulate-conditions evaluate-probability simulate-event
           simulate-effect simulate-effects simulate-event))

(defpackage :map-tools
  (:use :cl :cl-user)
  (:export game-map map-room-index map-room-visited map-room-description
           map-room-hidden-objects map-room-objects map-room-entities
           map-room-connections
           extend-map get-room gen-connection))

(defpackage :parse
  (:use :cl)
  (:export read-commands))

(defpackage :stork
  (:use :cl)
  (:export main))

(defun use-packages (package packages-to-use)
  (dolist (dependency packages-to-use)
    (use-package package dependency)))

(use-package '(:map-tools :event) :entity)
(use-package '(:event) :logic)
(use-package '(:logic :map-tools :entity) :event)
(use-package '(:event) :map-tools)
(use-package '(:map-tools :event :entity) :parse)
(use-package '(:parse :event) :stork)
