(ql:quickload "fiveam")
(load (merge-pathnames "connect-n.lisp" (user-homedir-pathname)))

(defpackage :test-package
  (:use :cl :fiveam)) ; Use :cl for common Lisp symbols

(defun generate-matrix (rows cols)
  (make-array (list rows cols)
              :initial-contents
              (loop for i below rows
                    collect (loop for j below cols
                                  collect (+ (* i cols) j)))))
(in-package :test-package)

(def-suite* test-matrix-traverse)
(test my-test
      (is (= '((0 6) (5) (1 7) (2 8) (3 5 9) (4)) (diagonal-order-neg (generate-matrix 2 5) 2 5))))
