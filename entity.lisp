;;;; entity.lisp

(in-package #:entity)

;; Define tendencies for hostile characters
(defconstant +hostility+
  `(('passive . nil)
    ('neutral . nil)
    ('hostile . nil)))

;; Define properties of senses
(defconstant +senses+
  `(('sight . nil)
    ('hearing . nil)
    ('taste . nil)
    ('touch . nil)
    ('smell . nil)))

;; Define tendencies for different races
(defconstant +races+
  '(('human . nil)
    ('dwarf . nil)
    ('elf . nil)
    ('orc . nil)
    ('goblin. nil)))

;; Define properties of a character
(defstruct person
  (name nil)
  (race nil)
  (hostility nil)
  (appearance nil)
  (clothes nil)
  (age 0)
  (birth-tick 0)
  (history nil)
  (mood nil)
  (senses nil)
  (status nil)
  (personality nil)
  (desires nil)
  (attention nil))

;; Properties of object
(defstruct object
  (owner nil)
  (name "")
  (age 0)
  (birth-tick 0)
  (type nil))
