;;;; map.lisp

(in-package #:map)

(defconstant +room-probabilty+ 0.2)
(defconstant +direction-vectors+
  (('north . (1 0)) ('east . (0 1)) ('south . (-1 0)) ('west . (0 -1))))
(defconstant +directions+
  ('north 'east 'south 'west))
(defconstant +direction-opposites+
  (('north . 'south) ('east . 'west) ('south . 'north) ('west . 'east)))

(defparameter game-map (make-hash-map))

(defstruct junction
  (index '(0 0))
  (first nil)
  (second nil))

(defstruct room
  (index '(0 0))
  (description "")
  (objects nil)
  (entities nil)
  (connections (('north . nil) ('east . nil) ('south . nil) ('west . nil))))

(defun zip (a b)
  (mapcar #'list a b))

(defun vector-add (v1 v2)
  (mapcar #'+ v1 v2))

;;; Represent a map as a graph
(defun extend-map (parent game-map)
  "Generate a new room leading from a previous room"
  (dotimes (direction +directions+)
    (when (not (assoc direction (room-connections room)))
      (if (or (isroom? (gethash index gamemap)) (<= +room-probability+ (random 1.0)))
          (setf (cdr (assoc direction (room-connections room)))
                (gen-room parent direction))
          (setf (cdr (assoc direction (room-connections room)))
                (gen-connection parent direction))))))

(defun get-room (parent direction)
  (let ((index (vector-add (room-index parent) (assoc direct +direction-vectors+))))
    (if (gethash index game-map)
        (gethash index game-map)
        (let child (make-room
                    :index (vector-add (room-index parent)
                                       (assoc direction 'direction-vectors))
                    :description ""
                    :objects nil
                    :entities nil
                    :connections (('north . nil) ('east . nil) ('south . nil) ('west . nil)))
             (setf (assoc (assoc direction +direction-opposites+) (room-connections child))
                   parent)
             (gen-room-description parent child)
             (setf (gethash index game-map) child)))))


(defun gen-connection (parent direction)
  (let ((index (vector-add (room-index parent) (assoc direction +direction-vectors+)))
        (current (gethash index game-map)))
    (cond
      (isroom current) (progn
                         (setf (assoc direction
                                      (room-connections (gethash index game-map)))
                               current
                               (assoc (assoc direction +opposite-directions+)
                                      (room-connections current))
                               parent)
                         current)
      (isconnection current) nil
      (not current) (if (<= +room-probability+ (random 1.0))
                        nil
                        (gen-connection parent))))); choose a random direction
