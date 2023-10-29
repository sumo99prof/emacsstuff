(defvar *unfair-coin* 0)
(defvar *winner-coin* 0)

(defun flip-coin (num-flips)
  (let ((coin-seq (loop for i from 1 to num-flips collect (random 2))))
    (if (search (make-list num-flips :initial-element 0) coin-seq)
        (progn
          (incf *winner-coin*)
          (format t "The value of the winning coin is: ~{~a~^, ~}~%" coin-seq))
        (format t "The value of the losing coin is: ~{~a~^, ~}~%" coin-seq)))
  )

(defun main-func (coin-size num-flips unfair-coin winner-coin)
  (let* ((coin-num coin-size)
         (pulled-coin (random coin-num)))
    (if (eql pulled-coin 0)
        (progn
          (incf *unfair-coin*)
          (format t "Unfair coin~%"))
        (progn
          (flip-coin num-flips)
          (finish-output)))
    )
  )

(defun user-interact ()
  (terpri)
  (setf *unfair-coin* 0)
  (setf *winner-coin* 0)
  (princ "Enter size of the bag: ")
  (setq bag-size (read))
  (princ "How many flips in a row? ")
  (setq flips-row (read))
  (princ "Number of double-headed coins drawn? ")
  (setq weighted-thresh (read))
  (loop while (> weighted-thresh *unfair-coin*)
        do (main-func bag-size flips-row)
        )
  (format t "The number of unfair coins ~d, the number of winner coins ~d" *unfair-coin*  *winner-coin*)
  )
