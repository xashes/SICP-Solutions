;; ex1.7
;; -> real? real? boolean?
(defun good-enough? (guess new-guess)
	(< (abs (- 1
		   (/ guess new-guess)))
	   0.001)
	)

;; -> real? real?
(defun sqrt-nt (x)
  (defun improve-guess (guess)
    (/ (+ guess 
	  (/ x guess))
       2))
  (defun sqrt-iter (guess)
    (let ((new-guess (improve-guess guess)))
      (if (good-enough? guess new-guess)
	  guess
	(sqrt-iter new-guess))))
  
  (sqrt-iter 1.0)
  )

(ert-deftest sqrt-nt-test ()
  (should (= (round (sqrt-nt 9)) 3.0))
  (should (= (round (sqrt-nt 36)) 6))
  )

;; ex1.8
;; -> real? real?
(defun cube-root (x)
  (defun improve-guess (guess)
    (/ (+ (* 2 guess)
	  (/ x (expt guess 2)))
       3.0))
  (defun cube-root-iter (guess)
    (let ((new-guess (improve-guess guess)))
      (if (good-enough? guess new-guess)
	  guess
	(cube-root-iter new-guess))
      ))
  (cube-root-iter 1.0)
  )

(ert-deftest cube-root-test ()
  (should (= (round (cube-root 1)) 1))
  (should (= (round (cube-root 27)) 3))
  )
