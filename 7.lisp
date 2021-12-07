(defparameter *crab-pos* (uiop:read-file-line "input/day7.txt"))

(defun crab-pos-list (crabs)
  (mapcar #'parse-integer (uiop:split-string crabs :separator ",")))

(defun difference (n crab)
  (abs (- n crab)))

(defun gauss-sum (n crab)
  (let ((num (difference n crab)))
    (/ (* (+ 1 num) num) 2)))

(defun sum (lst)
  (reduce #'+ lst))

(defun solve-problem (crabs)
  (let* ((sorted-crabs (stable-sort (crab-pos-list crabs) #'<=))
         (min-crab (first sorted-crabs))
         (max-crab (first (last sorted-crabs))))
    (loop for i from min-crab to max-crab
          minimize (sum (mapcar (lambda (n)
                                  ;; first part is `difference`
                                  ;; second part is `gauss-sum`
                                  (gauss-sum n i))
                                sorted-crabs)))))
