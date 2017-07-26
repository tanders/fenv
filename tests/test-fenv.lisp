(in-package :fe)

;; tests below outdated. Necessary changes:
;; - linear-env, sin-env, sin-env1 and expon-env are now functions (named with fenv, not env) that expect a list of points
;; OK - all occurances of env have been replaced by fenv
;; OK - plot is not defined (BPF is used instead)

;
;(defun test1 (x &key (min 0) (max 1))
;  (+ min (* x (- max min))))
;(dotimes (i 11)
;  (print (test1 (* i 0.1) :min -2 :max 1)))
;
;(dotimes (i 11)
;  (print 
;   (funcall (fenv #'identity :min 0.3 :max 0.7)
;            (* i 0.1))))
;
;(dotimes (i 11)
;  (print
;   (funcall (fenv1 #'identity :min 0.3 :max 0.7)
;            (funcall (fenv #'identity :min 0.3 :max 0.7)
;                     (* i 0.1)))))


;; depends on Opusmodus
(defun plotter (data &optional (number 100))
  (list-plot (cond ((fenv? data) (fenv->vector data number))
                   ((and (listp data) (every #'fenv? data))
                    (mapcar #'(lambda (xs) (fenv->vector xs number)) data))
                   (T data))
	     :join-points T :point-radius 2))


;; examples


(plotter (make-fenv #'sin :min (- pi) :max pi))

(plotter
  (funcs->fenv (list #'identity
		    #'identity
		    #'identity)
	      :min 1 :max 5))

(plotter (fenv-seq (make-fenv #'sin :max pi)
	   0.1 (make-fenv #'sin :max pi)
	   0.4 (make-fenv #'sin :max pi)))

(plotter (osciallator (make-fenv #'identity) 5))
	       
  
(plotter (linear-fenv (0 1) (0.2 3) (0.7 2) (1 -1)))

(plotter (linear-fenv (0 0) (1 2)))

(plotter (sin-fenv (0 1) (0.2 5) (0.3 -2) (0.8 7) (1 1)))

(plotter (sin-fenv (0 1) (0.2 0) (0.3 1) (0.8 0) (1 1)))

(plotter (sin-fenv1 (0 1) (0.2 5) (0.3 -2) (0.8 7) (1 1)))
      
(plotter (sin-fenv1 (0 1) (0.2 0) (0.3 1) (0.8 0) (1 1)))


;; !! not OK yet
(plotter (expon-fenv (0 1) (0.2 5) (0.3 2) (0.8 7) (1 1)))


(plotter (combine-fenvs #'+
		    10
		    (linear-fenv (0 0) (1 2))
		    (make-fenv #'sin :max (* pi 2))
		    (make-fenv #'sin :max pi)))


(plotter (linear-fenv (0 0) (0.7 2) (1 1)))
(plotter (reverse-fenv (linear-fenv (0 0) (0.7 2) (1 1))))

(plotter (saw 3))
(plotter (saw1 3 :amplitude 3 :offset 1))

(plotter (triangle 3 :amplitude 3 :offset 1))

(plotter (square 3 :amplitude 3 :offset 1))

(plotter (steps 1 2 3 2 1))

(plotter (random-steps 20 :min-y -10 :max-y -1)))

(plotter (random-steps 20 :min-y -1 :max-y 3))

(plotter (combine-fenvs #'*
		    (triangle 7)
		    (linear-fenv (0 0) (0.3 1) (1 0))))


(plotter 
 (scale-fenv 
  (make-fenv #'(lambda (x)
		(let ((exp 1000))
		  (/ (expt exp x) exp))))
  0.2 0.1))

;;
(let ((fenv (make-fenv #'sin :max pi)))
  (plotter (mapcar #'(lambda (fenv2)
		    (waveshape fenv fenv2))
		(list
		 (make-fenv #'identity) ; no waveshaping
		 (make-fenv #'(lambda (x) (/ (expt 2 x) 32))
			   :min -5 :max 5)
		 (make-fenv #'cos
			   :max (* pi 0.5))
		 fenv))))

(plotter (rising-expon-fenv 10))

(plotter 
 (scale-fenv (rising-expon-fenv 10)
	    3 0.1))

#| ; depends on Common Music
;; convert a fenv into a CM pattern (load in-cm.lisp first)
(next (make-fenv-pattern (fe::sin-fenv1 (0 0) (0.7 1) (1 0.5))
			 (new cycle of '(20 10)))
      50)
|#
