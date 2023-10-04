(defconst ranges
  '(;; Low
    (60 90 "blue")
    ;; Normal
    (80 120 "green")
    ;; Pre hypertension
    (90 140 "yellow")
    ;; High stage 1 hypertension
    (100 160 "orange")
    ;; High stage 2 hypertension
    (120 190 "red")))

(defun cell-color (diastolic systolic)
  (or (and diastolic systolic
           (cl-loop for (a b color) in ranges
                    when (and (<= diastolic a) (<= systolic b))
                      return color))
      "gray"))
