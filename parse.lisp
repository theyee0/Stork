;;;; parse.lisp

(in-package #:parse)

(defparameter +sentence-structures+
  '((verb)
    (verb noun)
    (verb noun preposition noun)))

;; Verb table is either of the form
;; (verb . (action . (parameters)))

(defparameter +prepositions+
  '(:with :to :at :for :above :below))

(defparameter +synonyms+
  '((:move . (:go :walk))
    (:attack . (:hit :strike))
    (:run . (:rush :dash))
    (:use . (:operate))))


(defparameter +verbs+
  '((:wait . (entity:wait . nil))
    (:move . (entity:move . nil))
    (:attack . (entity:attack . nil))
    (:search . (entity:search . nil))
    (:tiptoe . (entity:tiptoe . nil))
    (:run . (entity:run . nil))
    (:look . (entity:look . nil))
    (:use . (entity:use . nil))))

(defun split-command (command)
  (let ((space-index (position #\Space command)))
    (if space-index
        (if (equal (subseq command 0 space-index) " ")
            (split-command (subseq command (1+ space-index)))
            (cons (intern (subseq command 0 space-index) "KEYWORD") (split-command (subseq command (1+ space-index)))))
        (list (intern command "KEYWORD")))))

(defun match-verb (verb)
  (if (assoc verb +verbs+)
      t
      (progn
        (loop :for synonym :in +synonyms+ :do
          (when (find verb (cdr synonym))
            (return t)))
        nil)))

(defun match-sentence (command sentence-structure)
  (if (or (not command) (not sentence-structure))
      (and (not command) (not sentence-structure))
      (case (car sentence-structure)
        (verb (and (match-verb (car command))
                   (match-sentence (cdr command) (cdr sentence-structure))))
        (noun (match-sentence (cdr command) (cdr sentence-structure)))
        (preposition (and (find (car command) +prepositions+)
                          (match-sentence (cdr command) (cdr sentence-structure)))))))

(defun run-command (state command sentence-structure)
  (if (or (not command) (not sentence-structure))
      nil
      (case (car sentence-structure)
        (verb (apply (cadr (assoc (car command) +verbs+))
                     state
                     (concatenate 'list (cddr (assoc (car command) +verbs+))
                                  (run-command state (cdr command) (cdr sentence-structure)))))
        (noun (cons (car command) (run-command state (cdr command) (cdr sentence-structure))))
        (preposition (cons (car command) (run-command state (cdr command) (cdr sentence-structure)))))))

(defun read-commands (state command)
  (let ((words (split-command (string-upcase command))))
    (loop :for structure :in +sentence-structures+ :do
      (when (match-sentence words structure)
        (format t "Interpreting sentence structure as ~a: ~a~%" structure command)
        (run-command state words structure)
        (return-from read-commands t))))
  (format t "Failed to identify sentence structure of ~S~%" command))

