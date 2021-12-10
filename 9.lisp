(ql:quickload :alexandria)

(defparameter *map* (uiop:read-file-lines "input/day9.txt"))

(defparameter *map-len* (length *map*))

(defun map-to-lists (map)
  (mapcar #'(lambda (l)
              (loop for i across l
                    collect (digit-char-p i)))
          map))

(defun push-extend (item place)
  (setf place (push item
                    (cdr (nthcdr (1- (length place))
                                 place)))))

(defun up-and-down-boundaries (map)
  (let ((ten-list (make-list 100 :initial-element 10)))
    (loop for ln in map
          do (push-extend 10 ln))
    (push ten-list map)
    (push-extend ten-list map)
    (alexandria:flatten map)))

(defun low-point-p (idx old-map new-map)
  (let ((n (elt old-map idx))
        (left (elt new-map (1- idx)))
        (right (elt new-map (1+ idx)))
        (up (elt new-map (- idx 100)))
        (down (elt new-map (+ idx 100))))
    (every #'identity
           (mapcar (lambda (x)
                     (< n x))
                   (list left right up down)))))

(defun solve-first-part (map new-map)
  (loop with map-lists = (map-to-lists map)
        with new-map = (up-and-down-boundaries map-lists)
        for idx from 0 below (expt (length map-lists) 2)
        ;; for i in (alexandria:flatten map-lists)
        with low-point = (low-point-p idx old-map new-map)
        if (eql t low-point)
          collect low-point into low-points
        finally (return (length low-points)
                        (reduce #'+ low-points))))
