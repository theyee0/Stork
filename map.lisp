;;;; map.lisp

(defparameter game-map nil)

(defstruct room
  (description "")
  (objects nil)
  (entities nil)
  (north nil)
  (east nil)
  (south nil)
  (west nil))

;;; Represent a map as a graph

(defun gen-room (game-map)
  "Generate a new room leading from a previous room"
  room)
