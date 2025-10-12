;;;; parse.lisp

(in-package #:parse)

(defparameter +sentence-structures+
  '((verb noun)
    (verb noun preposition noun)))

;; Verb table is either of the form
;; (verb . (action . (parameters)))

(defparameter +prepositions+
  '(with to at for above below))

(defparameter +synonyms+
  '((move . (go walk))
    (attack . (hit strike))
    (rush . (run dash))
    (use . (operate))))
     

(defparameter +verbs+
  '((move . (#'move . nil))
    (attack . (#'attack . nil))
    (search . (#'search . nil))
    (tiptoe . (#'sneak . nil))
    (run . (#'rush . nil))
    (use . (#'use . nil))))

(defun split-command (command)
  (let ((space-index (position #\Space command)))
    (if space-index
        (if (equal (subseq command 0 space-index) "")
            (split-command (subseq command space-index))
            (cons (subseq command 0 space-index) (split-command (subseq command space-index))))
        (list command))))

(defun match-verb (verb)
  (if (assoc verb +verbs+)
      (cdr (assoc verb +verbs+))
      (progn
        (loop :for synonym :in +synonyms+ :do
          (when (assoc synonym +verbs+)
            (return (cdr (assoc synonym +verbs+)))))
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
        (verb (apply (cdar command)
                     state
                     (cddr command)
                     (run-command state (cdr command) (cdr sentence-structure))))
        (noun (cons (car command) (run-command state (cdr command) (cdr sentence-structure))))
        (preposition (cons (car command) (run-command state (cdr command) (cdr sentence-structure)))))))

(defun read-commands (state command)
  (let ((words (split-command command)))
    (loop :for structure :in +sentence-structures+ :do
      (when (match-sentence words structure)
        (run-command state words structure)
        (return))))
  (format t "Failed to identify sentence structure of ~S" command))
      
