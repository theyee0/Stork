;;;; event.lisp

(defstruct context
  (current-room nil)
  (event-stack nil)
  (status nil)
  (objects nil)
  (entities nil))

;;; Table and constant definition
(defconstant +event-table+
  `((('type . 'feeling)
     ('name . "A ~A falls upon you..."))
    (('type . 'status-effect
     ('name . "The ~A ~A... you feel ~A")))
    (('type . 'appearance)
     ('name . "A ~A appeared ~A from ~A!"))))

(defconstant +events-per-tick+ 3)

;;; Top-Level functions
(defun simulate-events (state event-table)
  "Simulate a given number of events based on the current game state"
  (dotimes (i +events-per-tick+)
      (loop for event in event-table do
            (evaluate-event state event))))

(defun evaluate-probability (state event)
  "Compute the probability of an event taking place"
  (let (probabiltity-list (assoc 'prob event))
    (reduce #'+ (map 'list
                     #'(lambda (x) (condition-met-p state x))
                     (assoc '(probability event))))))

(defun evaluate-event (state event)
  "Sum the probability of an event happening and if so, execute the event"
  (let (prob (evaluate-probability state event))
    (when (<= (random 1.0) prob)
      (simulate-event state event)))))

(defun simulate-event (state event)
  "Run an event unconditionally"
  (format t "~S\n~S" (assoc 'name event) (assoc 'description event))
  (loop for action in (assoc 'results event)
        (action state)))

(defun condition-met-p (state condition)
  "Ensure that a given condition is met"
  (gethash condition (context-status state)))
