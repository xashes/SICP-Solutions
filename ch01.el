;; ex1.7
(defun sqrt-nt (x)
  (defun improve-guess (guess)
    (/ (+ guess 
	  (/ x guess))
       2))
  (defun sqrt-iter (guess)
    (let ((new-guess (improve-guess guess)))
      (defun good-enough? (guess)
	(< (abs (- 1
		   (/ guess new-guess)))
	   0.001))
      (if (good-enough? guess)
	  guess
	(sqrt-iter new-guess))))
  
  (sqrt-iter 1.0)
  )

(ert-deftest sqrt-nt-test ()
  (should (= (round (sqrt-nt 9)) 3.0))
  (should (= (round (sqrt-nt 36)) 6))
  )
