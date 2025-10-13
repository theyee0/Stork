;;;; event.lisp

(in-package #:event)

(defconstant +ticks-per-event+ 17)
(defconstant +ticks-per-second+ 1)
(defconstant +ticks-per-minute+ (* 60 +ticks-per-second+))
(defconstant +ticks-per-hour+ (* 60 +ticks-per-minute+))
(defconstant +ticks-per-day+ (* 24 +ticks-per-hour+))
(defconstant +ticks-per-month+ (* 30 +ticks-per-day+))
(defconstant +ticks-per-year+ (* 12 +ticks-per-month+))

(defstruct context
  (tick 0)
  (self nil)
  (player (make-person))
  (game-map (make-hash-table))
  (current-room nil)
  (event-stack nil)
  (status (make-hash-table))
  (objects (make-hash-table))
  (entities (make-hash-table)))

(defun check-status (state symb)
  (gethash symb (context-status state)))

(defun initial-room (state)
  (equal (map-room-index (context-current-room state)) '(0 0)))

(defun not-initial-room (state)
  (not (initial-room state)))

;; Table format:
;; ('type-1 . (('property-1 . 'value-1)
;;             ('property-2 . 'value-2)
;;             ...))
;; ('type-2 . (('property-3 . ('effect . (('func-1 . (((parameters) . probability)
;;                                                    ...))
;;                                        ('func-2 . (((parameters) . probability)
;;                                                    ...))
;;                                        ...)))
;;             ...))


(defparameter +feelings+
  `((:dread . ((:excitement . "...")
              (:condition . ((check-status . (((:foreboding) . 0.1)
                                             ((:silence) . 0.8)
                                             ((:cold) . 0.3)
                                             ((:death) . 0.6)
                                             ((:grime) . 0.3)
                                             ((:darkness) . 0.5)))
                            (not-initial-room . ((nil . 0.3)))))
              (:effect . ((change-person . (((:mood :poor) . 0.9)
                                           ((:concentration :poor) . 0.1)))))))
    (:foreboding . ((:excitement . "...")
                   (:condition . ((check-status . (((:foreboding) . 0.7)
                                                  ((:death) . 0.7)
                                                  ((:grime) . 0.2)
                                                  ((:darkness) . 0.4)))
                                 (not-initial-room . ((nil . 0.3)))))
                   (:effect . ((change-person . (((:mood :poor) . 0.6)
                                                ((:concentration :poor) . 0.7)))))))
    (:ecstasy . ((:excitement . "!!!")
                (:condition . ((check-status . (((:beauty) . 0.3)
                                               ((:warmth) . 0.7)
                                               ((:comfort) . 0.8)
                                               ((:life) . 0.8)
                                               ((:cleanliness) . 0.4)
                                               ((:light) . 0.4)))
                              (not-initial-room . ((nil . 0.1)))))
                (:effect . ((change-person . (((:mood :good) . 1.0)
                                             ((:concentration :good) . 0.6)))))))
    (:rage . ((:excitement . "!")
             (:condition . ((check-status . (((:betrayal) . 0.8)
                                            ((:hatred) . 0.4)
                                            ((:conflict) . 0.3)
                                            ((:war) . 0.3)))
                           (not-initial-room . ((nil . 0.3)))))
             (:effect . ((change-person . (((:mood :poor) . 0.7)
                                          ((:concentration :poor) . 0.3)))))))
    (:anger . ((:excitement . "!")
              (:condition . ((check-status . (((:betrayal) . 0.9)
                                             ((:hatred) . 0.5)
                                             ((:conflict) . 0.5)
                                             ((:war) . 0.3)))
                            (not-initial-room . ((nil . 0.3)))))
              (:effect . ((change-person . (((:mood :poor) . 0.7)
                                           ((:concentration :poor) . 0.3)))))))
    (:joy . ((:excitement . "!")
            (:condition . ((check-status . (((:beauty) . 0.4)
                                           ((:warmth) . 0.2)
                                           ((:comfort) . 0.2)
                                           ((:life) . 0.4)
                                           ((:cleanliness) . 0.2)
                                           ((:light) . 0.2)))
                          (not-initial-room . ((nil . 0.3)))))
            (:effect . ((change-person . (((:mood :good) . 0.9)
                                         ((:concentration :good) . 0.7)))))))
    (:fear . ((:excitement . "!?")
             (:condition . ((check-status . (((:foreboding) . 0.4)
                                            ((:death) . 0.9)
                                            ((:grime) . 0.1)
                                            ((:dark) . 0.7)))
                           (not-initial-room . ((nil . 0.3)))))
             (:effect . ((change-person . (((:mood :poor) . 0.7)
                                          ((:concentration :poor) . 0.3)))))))
    (:contentedness . ((:excitement . ".")
                      (:condition . ((check-status . (((:beauty) . 0.8)
                                                     ((:warmth) . 0.9)
                                                     ((:comfort) . 0.8)
                                                     ((:light) . 0.3)))
                                    (not-initial-room . ((nil . 0.3)))))
                      (:effect . ((change-person . (((:mood :good) . 0.5)
                                                   ((:concentration :good) . 1.0)))))))))

;; TODO: Add behaviors
(defparameter +status-effects+
  `((:dizzy . ((:excitement . "...")
              (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
              (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:confused . ((:excitement . "...?")
                 (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
                 (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:strengthened . ((:excitement . "!")
                     (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
                     (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:empowered . ((:excitement . "!")
                  (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
                  (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:weakened . ((:excitement . "...")
                 (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
                 (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))))

;; TODO: Add behaviors
(defparameter +apparitions+
  `((:goose . ((:excitement . "!!!!!!!!!!!!!!!!!!")
              (:origin . ((,(lambda (x) T) . ((nil . 0.1)))))
              (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
              (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:stork . ((:excitement . "...?")
              (:origin . ((,(lambda (x) T) . ((nil . 0.1)))))
              (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
              (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))
    (:wisp . ((:excitement . "...")
             (:origin . ((,(lambda (x) T) . ((nil . 0.1)))))
             (:condition . ((,(lambda (x) T) . ((nil . 0.1)))))
             (:effect . ((,(lambda (x) T) . ((nil . 0.1)))))))))

(defmacro generate-variants (constants)
  (map 'list #'car constants))

;;; Table and constant definition
(defparameter +event-classes+
  `((:feeling . ((:name . "A feeling of ~A falls upon you~A")
                (:parameters . (:name :excitement))
                (:variants . ,+feelings+)))
    (:status-effect . ((:name . "You feel ~A~A")
                      (:parameters . (:name :excitement))
                      (:variants . ,+status-effects+)))
    (:apparation . ((:name . "A ~A appeared~A")
                   (:parameters . (:name :excitement))
                   (:variants . ,+apparitions+)))
    (:luck . ((:name . "Somehow you feel vaguely optimistic, like something might go well...")))
    (:unluck . ((:name . "You feel vaguely pessimistic. You feel worried that something will go poorly...")))))

(defmacro generate-event-from-class (event-classes const-name)
  `(defparameter ,const-name
     (apply #'concatenate 'list
            (map 'list
                 (lambda (event-class)
                   (let ((variants (cdr (assoc :variants (cdr event-class))))
                         (name (cdr (assoc :name (cdr event-class))))
                         (parameters (cdr (assoc :parameters (cdr event-class)))))
                     (map 'list
                          (lambda (variant)
                            `(,(car variant) . ((:name . ,(apply #'format nil name
                                                                (map 'list
                                                                     (lambda (key)
                                                                       (if (and (eql key :name) (not (assoc :name (cdr variant))))
                                                                           (string-downcase (string (car variant)))
                                                                           (cdr (assoc key (cdr variant)))))
                                                                     parameters)))
                                                ,(assoc :condition (cdr variant))
                                                ,(assoc :effect (cdr variant)))))
                          variants)))
                 ,event-classes))))

(generate-event-from-class +event-classes+ events)

(defun get-time-unit (state granularity maximum)
  (let ((units (floor (/ (context-tick state) granularity))))
    (if (<= maximum 0)
        units
        (mod units maximum))))

(defun get-date (state)
  (let ((second (get-time-unit state +ticks-per-second+ 60))
        (minute (get-time-unit state +ticks-per-minute+ 60))
        (hour (get-time-unit state +ticks-per-hour+ 24))
        (day (get-time-unit state +ticks-per-day+ 30))
        (month (get-time-unit state +ticks-per-month+ 12))
        (year (get-time-unit state +ticks-per-year+ -1)))
    (format nil "In the year ~A, in day ~A of month ~A at ~2,'0D:~2,'0D:~2,'0D"
            year day month hour minute second)))

;;; Top-Level functions
(defun simulate-events (state event-table)
  "Simulate a given number of events based on the current game state"
  (format t "~A~%" (get-date state))
  (incf (context-tick state) +ticks-per-event+)
  (dotimes (i 2)
    (dolist (event event-table) (evaluate-event state event))))

(defun evaluate-conditions (state event)
  (- 1 (reduce #'*
               (map 'list
                    (lambda (situation)
                      (- 1 (evaluate-probability state situation)))
                    (cdr (assoc :condition (cdr event)))))))

(defun evaluate-probability (state probability-list)
  "Compute the probability of an event taking place"
  (let ((probability-function (car probability-list))
        (situations (cdr probability-list)))
    (- 1 (reduce #'*
                 (map 'list
                      (lambda (situation)
                        (if (apply probability-function state (car situation))
                            (- 1 (cdr situation))
                            1))
                      situations)))))

(defun evaluate-event (state event)
  "Sum the probability of an event happening and if so, execute the event"
  (let ((prob (evaluate-conditions state event)))
    (when (<= (random 1.0) prob)
      (simulate-event state event))))

(defun simulate-effect (state effect)
  (let ((effect-function (car effect))
        (situations (cdr effect)))
    (dolist (situation situations)
      (let ((probability (cdr situation))
            (parameters (car situation)))
        (if (<= probability (random 1.0))
            (apply effect-function state parameters))))))

(defun simulate-effects (state event)
  (let ((effects (cdr (assoc :effect (cdr event)))))
    (dolist (effect effects)
      (simulate-effect state effect))))

(defun simulate-event (state event)
  "Run an event unconditionally"
  (format t "~A~%" (cdr (assoc :name (cdr event))))
  (simulate-effects state event))
