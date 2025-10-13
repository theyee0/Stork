;;;; entity.lisp

(in-package #:entity)

;; Define tendencies for hostile characters
(defparameter +hostility+
  `(('passive . nil)
    ('neutral . nil)
    ('hostile . nil)))

;; Define properties of senses
(defparameter +senses+
  `(('sight . nil)
    ('hearing . nil)
    ('taste . nil)
    ('touch . nil)
    ('smell . nil)))

;; Define tendencies for different races
(defparameter +races+
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
  (skills (make-hash-table))
  (senses nil)
  (status nil)
  (personality nil)
  (desires nil)
  (attention nil))

;; Properties of object
(defstruct object
  (owner nil)
  (containments nil)
  (behaviors nil)
  (name "")
  (age 0)
  (birth-tick 0)
  (type nil))

(defun wait (state)
  (format t "You wait in place. ~A~%" (map-room-description (context-current-room state))))

(defun move (state direction)
  (let ((destination (get-room state (context-current-room state) direction))
        (current (context-current-room state)))
    (if (not destination)
        (format t "There is nothing to the ~a.~%" (string-downcase (string direction)))
        (progn
          (when (not (map-room-visited destination))
            (setf (map-room-visited destination) t)
            (extend-map state destination))
        (setf current destination)))))

(defun attack (state target)
  (let ((entities (context-entities state)))
    (if (gethash target entities)
        (format t "Oops! You tried to attack something, which is not yet implemented.~%")
        (format t "That target does not exist.~%"))))

(defun search-room (state target)
  (let* ((current-room (context-current-room state))
         (objects (map-room-objects current-room))
         (target-object (gethash target objects)))
    (if target-object
        (let* ((skills (person-skills (context-player state)))
              (search-skill (gethash 'search skills)))
          (look state)
          (if (and search-skill (<= search-skill (random 1.0)))
              (progn
                (format t "Inside, there is: ")
                (dolist (containment (object-containments target-object))
                  (format t "~a, " (object-name containment)))
                (format t "~%"))
              (format t "You couldn't find anything.~%")))
        (format t "That target does not exist.~%"))))

; TODO: Add quietness as characteristic
(defun tiptoe (state direction)
  (move state direction))

; TODO: Add speed as characteristic
(defun run (state direction)
  (move state direction))

(defun look (state)
  (format t "~a~%" (map-room-description (context-current-room state))))

(defun use (state object)
  (if (object-behaviors object)
      (dolist (behavior (object-behaviors object))
        (funcall behavior state))
      (format t "~a doesn't do anything.~%" (object-name object))))
