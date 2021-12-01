(defparameter *numbers*
  (mapcar #'parse-integer (uiop:read-file-lines "input/day1.txt")))

;; Part I

(defun compare-numbers (numbers)
  (loop for (i j) on numbers
        while j
        count (> (- j i) 0)))

(print (compare-numbers *numbers*))

;; Part II

(defun sum-of-windows (numbers)
  (loop for (x y z) on numbers
        while z
        collect (+ x y z)))

(print (compare-numbers (sum-of-windows *numbers*)))
