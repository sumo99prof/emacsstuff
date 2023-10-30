(defvar *unfair-coin* 0)
(defvar *winner-coin* 0)

(defun prompt-and-read (message)
  (princ message)
  (read))

(defun flip-coin (num-flips)
  (let ((coin-seq (loop for i from 1 to num-flips collect (random 2))))
    (if (search (make-list num-flips :initial-element 0) coin-seq)
        (progn
          (incf *winner-coin*)
          (format t "The value of the winning coin is: ~{~a~^, ~}~%" coin-seq))
        (format t "The value of the losing coin is: ~{~a~^, ~}~%" coin-seq))))

(defun main-func (coin-size num-flips)
  (let ((pulled-coin (random coin-size)))
    (if (eql pulled-coin 0)
        (progn
          (incf *unfair-coin*)
          (format t "Unfair coin~%"))
        (progn
          (flip-coin num-flips)
          (finish-output)))))

(defun user-interact ()
  (terpri)
  (setf *unfair-coin* 0)
  (setf *winner-coin* 0)
  (let ((bag-size (prompt-and-read "How many coins in the bag?"))
        (flips-row (prompt-and-read "How many flips in a row?"))
        (weighted-thresh (prompt-and-read "Number of double-headed coins drawn?")))
    (loop while (> weighted-thresh *unfair-coin*)
          do (main-func bag-size flips-row))
    (format t "The number of unfair coins ~d, the number of winner coins ~d" *unfair-coin* *winner-coin*)))
