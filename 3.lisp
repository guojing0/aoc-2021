(defparameter *report* (uiop:read-file-lines "input/day3.txt"))

(defparameter *report-length* (length *report*))
(defparameter *number-length* (length (car *report*)))

(defun majority-bit (nth report)
  (if (> (count #\0 (mapcar #'(lambda (seq) (elt seq nth))
                            report))
         (/ *report-length* 2))
      #\0
      #\1))

(defun gamma-rate (num-len report)
  (parse-integer (coerce (loop for index from 0 below num-len
                               collect (majority-bit index report))
                         'string)
                 :radix 2))

(defun epsilon-rate (num-len gamma-rate)
  (logxor gamma-rate
          (parse-integer (make-string num-len :initial-element #\1) :radix 2)))

(defun solve-first-part (num-len report)
  (let ((gamma (gamma-rate num-len report)))
    (* gamma (epsilon-rate num-len gamma))))

;; Part II

(defun oxygen-bit-criteria (nth report)
  (if (>= (count #\1 (mapcar #'(lambda (seq) (elt seq nth))
                             report))
          (/ (length report) 2))
      #\1
      #\0))

(defun oxygen-rating (num-len report)
  (loop with report = (copy-seq report)
        for index from 0 below num-len
        do (progn (setf report (remove-if-not
                                #'(lambda (seq)
                                    (char-equal (oxygen-bit-criteria index report)
                                                (elt seq index)))
                                report)))
        until (= 1 (length report))
        finally (return (parse-integer (car report) :radix 2))))

(defun CO2-bit-criteria (oxygen-bit)
  (if (char-equal #\1 oxygen-bit) #\0 #\1))

(defun CO2-rating (num-len report)
  (loop with report = (copy-seq report)
        for index from 0 below num-len
        do (progn (setf report (remove-if-not
                                #'(lambda (seq)
                                    (char-equal (co2-bit-criteria
                                                 (oxygen-bit-criteria index report))
                                                (elt seq index)))
                                report)))
        until (= 1 (length report))
        finally (return (parse-integer (car report) :radix 2))))

(defun solve-second-part ()
  (* (oxygen-rating *number-length* *report*)
     (co2-rating *number-length* *report*)))
