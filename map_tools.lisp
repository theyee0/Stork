;;;; map_tools.lisp

(in-package #:map-tools)

(defparameter +room-probability+ 0.2)
(defparameter +direction-vectors+
  `((:north . (1 0)) (:east . (0 1)) (:south . (-1 0)) (:west . (0 -1))))
(defparameter +directions+
  `(:north :east :south :west))
(defparameter +direction-opposites+
  `((:north . :south) (:east . :west) (:south . :north) (:west . :east)))

(defstruct junction
  (index '(0 0))
  (first nil)
  (second nil))

(defstruct map-room
  (visited nil)
  (index '(0 0))
  (description "")
  (hidden-objects (make-hash-table))
  (objects (make-hash-table))
  (entities (make-hash-table))
  (connections `((:north . nil) (:east . nil) (:south . nil) (:west . nil))))
(defun zip (a b)
  (mapcar #'list a b))

(defun vector-add (v1 v2)
  (mapcar #'+ v1 v2))

;;; Represent a map as a graph
(defun extend-map (state parent)
  "Generate a new room leading from a previous room"
  (dolist (direction +directions+)
    (when (not (cdr (assoc direction (map-room-connections parent))))
      (setf (cdr (assoc direction (map-room-connections parent)))
            (gen-connection state parent direction)))))

(defun get-room (state parent direction)
  (let ((index (vector-add (map-room-index parent) (cdr (assoc direction +direction-vectors+))))
        (game-map (context-game-map state)))
    (if (gethash index game-map)
        (gethash index game-map)
        (let ((child (make-map-room
                      :index (vector-add (map-room-index parent)
                                         (cdr (assoc direction +direction-vectors+)))
                      :description ""
                      :objects nil
                      :entities nil
                      :connections (list '(:north . nil) '(:east . nil) '(:south . nil) '(:west . nil)))))
          (setf (cdr (assoc (cdr (assoc direction +direction-opposites+)) (map-room-connections child)))
                parent)
          (gen-room-description parent child)
          (setf (gethash index game-map) child)))))

;; TODO: Create proper event-based room generation
(defun gen-room-description (parent child)
  (setf (map-room-description child) (map-room-description parent)
        (map-room-objects child) (map-room-objects parent)
        (map-room-entities child) (map-room-entities parent)))

(defun gen-connection (state parent direction)
  (let* ((index (vector-add (map-room-index parent) (cdr (assoc direction +direction-vectors+))))
         (game-map (context-game-map state))
         (current (gethash index game-map)))
    (cond
      ((map-room-p current) (progn
                              (setf (cdr (assoc direction
                                                (map-room-connections (gethash index game-map))))
                                    current
                                    (cdr (assoc (cdr (assoc direction +direction-opposites+))
                                                (map-room-connections current)))
                                    parent)
                              current))
      ((junction-p current) nil)
      ((not current) (if (<= +room-probability+ (random 1.0))
                         nil
                         (gen-connection state parent (nth (random 4) +directions+)))))))
