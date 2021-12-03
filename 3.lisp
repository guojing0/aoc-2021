(defparameter *report* (uiop:read-file-lines "input/day3.txt"))

(defparameter *number-length* (length (car *report*)))

(defun majority-bit (nth report)
  (if (>= (count #\1 (mapcar #'(lambda (seq) (elt seq nth))
                                  report))
          (/ (length report) 2))
      #\1
      #\0))

(defun gamma-rate (num-len report)
  (parse-integer (coerce (loop for index from 0 below num-len
                               collect (majority-bit index report))
                         'string)
                 :radix 2))

(defun epsilon-rate (num-len gamma-rate)
  (logxor gamma-rate
          (parse-integer (make-string num-len :initial-element #\1) :radix 2)))

(defun solve-first-part ()
  (let ((gamma (gamma-rate *number-length* *report*)))
    (* gamma (epsilon-rate *number-length* gamma))))

;; Part II

(defun rating-template (criteria-fn num-len report)
  (loop with report = (copy-seq report)
        for index from 0 below num-len
        do (progn (setf report (remove-if-not
                                #'(lambda (seq)
                                    (char-equal (funcall criteria-fn index report)
                                                (elt seq index)))
                                report)))
        until (= 1 (length report))
        finally (return (parse-integer (car report) :radix 2))))

(defun oxygen-rating (num-len report)
  (rating-template #'majority-bit num-len report))

(defun CO2-rating (num-len report)
  (flet ((co2-bit-criteria (nth report)
           (if (char-equal #\1 (majority-bit nth report)) #\0 #\1)))
    (rating-template #'co2-bit-criteria num-len report)))

(defun solve-second-part ()
  (* (oxygen-rating *number-length* *report*)
     (co2-rating *number-length* *report*)))
