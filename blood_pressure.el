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

 #||
Original python implementation for reference

# Online Python - IDE, Editor, Compiler, Interpreter
if diastolic <= 60:
	if systolic <= 90:
		print("Low blood pressure")
	elif systolic <= 120:
		print("Ideal blood pressure")
	elif systolic <= 140:
		print("Pre high blood pressure")
	else:
		print("High blood pressure") 
elif diastolic <= 80:
	if systolic <= 120:
		print("Ideal blood pressure")
	elif systolic <= 140:
		print("Pre high blood pressure")
	else:
		print("High Blood Pressure")
elif diastolic <= 90:
    if systolic >= 140:
        print("High blood pressure")
    else:
        print("Pre high blood pressure")
else:
    print("High blood pressure")
||#
