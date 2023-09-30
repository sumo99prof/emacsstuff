(require 'cl-lib)

(defconst ranges
  '((60 90 "low")
    (80 120 "normal")
    (90 140 "pre hypertension")
    (100 160 "high stage 1 hypertension")
    (120 190 "high stage 2 hypertension")))

(defun blood-pressure (diastolic systolic)
  (cl-loop for (a b type) in ranges
           when (and (<= diastolic a) (<= systolic b))
             return type))

(blood-pressure 50 70) ;; => "low"
(blood-pressure 95 70) ;; => "high stage 1 hypertension"
(blood-pressure 85 130) ;; => "pre hypertension"
(blood-pressure 50 170)
