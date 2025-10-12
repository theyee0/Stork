;;;; map_tools.lisp

(in-package #:map-tools)

(defconstant +room-probability+ 0.2)
(defconstant +direction-vectors+
  `((north . (1 0)) (east . (0 1)) (south . (-1 0)) (west . (0 -1))))
(defconstant +directions+
  `(north east south west))
(defconstant +direction-opposites+
  `((north . south) (east . west) (south . north) (west . east)))

(defparameter game-map (make-hash-table))

(defstruct junction
  (index '(0 0))
  (first nil)
  (second nil))

(defstruct map-room
  (index '(0 0))
  (description "")
  (objects nil)
  (entities nil)
  (connections `((north . nil) (east . nil) (south . nil) (west . nil))))

(defun zip (a b)
  (mapcar #'list a b))

(defun vector-add (v1 v2)
  (mapcar #'+ v1 v2))

;;; Represent a map as a graph
(defun extend-map (parent game-map)
  "Generate a new room leading from a previous room"
  (dolist (direction +directions+)
    (when (not (cdr (assoc direction (map-room-connections parent))))
      (setf (cdr (assoc direction (map-room-connections parent)))
            (gen-connection parent direction)))))

(defun get-room (parent direction)
  (let ((index (vector-add (map-room-index parent) (cdr (assoc direction +direction-vectors+)))))
    (if (gethash index game-map)
        (gethash index game-map)
        (let ((child (make-map-room
                      :index (vector-add (map-room-index parent)
                                         (cdr (assoc direction +direction-vectors+)))
                      :description ""
                      :objects nil
                      :entities nil
                      :connections (list '(north . nil) '(east . nil) '(south . nil) '(west . nil)))))
          (setf (cdr (assoc (cdr (assoc direction +direction-opposites+)) (map-room-connections child)))
                parent)
          (gen-room-description parent child)
          (setf (gethash index game-map) child)))))

;; TODO: Create proper event-based room generation
(defun gen-room-description (parent child)
  (setf (map-room-description child) (map-room-description parent)
        (map-room-objects child) (map-room-objects parent)
        (map-room-entities child) (map-room-entities parent)))

(defun gen-connection (parent direction)
  (let* ((index (vector-add (map-room-index parent) (cdr (assoc direction +direction-vectors+))))
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
                         (gen-connection parent (nth (random 4) +directions+)))))))
