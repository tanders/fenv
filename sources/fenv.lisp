(in-package :fe)

;;;
;;; there is an example in ../tests/test-fenv.lisp for (hopefully) every function
;;;

;;;
;;; Defines means and abstractions to use numerical functions as envelopes
;;; and provides a rich set of functions to generate, combine and transform
;;; these envelopes.
;;; 

#| TODO:

- Def equivalent of method y for a BPF, once Mika or Mikael tell me how

- Define boxes with PWGLDef for all definitions, to have their documentation -- or wait for having the doc string of functions shown in PWGL.

- OK !! For all method defs that I want to use directly as boxes and that have optional args (e.g., keyword args) change them into functions (optional args are not supported for methods)

|#

;;;
;;; data structure
;;;

(defclass fenv ()
  ((fenv :accessor fenv 
	:initarg :fenv
	:initform (make-fenv #'identity)
	:type function
	:documentation "A unary numeric function, which defines an envelope in the function interval [0,1].")))

(defmethod fenv? ((x fenv))
  T)
(defmethod fenv? ((x T))
  NIL)

(defmethod y ((fenv fenv) (x number))
  "Access the y value of fenv at x."
  (funcall (fenv fenv) x))

;; TODO: 
;; - Hack using sampling -- replace this once I know how to access y values of PBFs directly
;; - once I know how to extract the min/max from a BPF then use those automatically
(defun BPF->fenv (BPF &key (number 100))
  "Samples a BPF and translates it into a fenv in [0, 1], which in turn linearily interpolates between samples. Number is number of samples."
  (let ((xs (loop for n from 0 to 1 by (/ 1 (1- number))
		  collect n))
	(ys (first (ccl::pwgl-sample BPF number))))
    (linear-fenv (pw::mat-trans (list xs ys)))))


(defun fenv->list (fenv &optional (number 100))
  "Samples a fenv from 0 to 1 (including) and collects samples in a list. Number is number of samples (if number=1, then the last fenv value is returned)."
  (if (= number 1) (list (y fenv 1))
    (loop for n from 0 to 1 by (/ 1 (1- number))
	  collect (y fenv n))))

; (fenv->list (make-fenv #'sin :max pi) 10)

;; BUG: always returns NIL -- seemingly the 1st argument needs to be something different
;; TODO:
;; - Decide x values to use for resulting BPFs. x values of fenvs are always in [0, 1] and x values of BPF are by default the sequence (0, 1, 2, ...). Which convention should I use?
;; - Add additional optional BPF args color and name
(defun fenv->BPF (fenv &optional (number 1000))
  "Samples a fenv from 0 to 1 (including) and translates it into a BPF, which in turn linearily interpolates between samples. Number is number of samples (if number=1, then the last fenv value is returned)."
  (ccl::2D-bpf-constructor :bpf (fenv->list fenv number)))
  

;;;
;;; generators
;;;

(defun make-fenv (func &key (min 0) (max 1))
  "Builds a fenv from a given numeric function, such that interval [0,1] ofthe resulting env-func is mapped to [min, max] of the argument function."
  (make-instance 'fenv
    :fenv #'(lambda (x)
	     (assert (<= 0 x 1))
	     (funcall func (+ min (* x (- max min)))))))

; (fenv? (make-fenv #'identity :min 1 :max 2))
 
(defun make-fenv1 (func &key (min 0) (max 1))
  "Builds a fenv from a given numeric function, such that interval [min,max] of resulting fenv is mapped to [0,1] of the argument function."
  (make-instance 'fenv
    :fenv #'(lambda (x)
	     (assert (<= min x max))
	     (funcall func (/ (-  x min) (- max min))))))

(defun points->fenv (func points)
  "Converts a list of points to a single fenv. A point is an x-y-pair as (xi yi). x values of the points range from 0--i (including), e.g., ((0 y1) (x2 y2) ... (1 yn)). The function defines the shape of the fenv segments and must return a fenv. It expects four numeric arguments, which describe the start and end points of the segment in the form (x1 y1 x2 y2)."
  (let* ((xs (mapcar #'first points))
	 (ys (mapcar #'second points))
	 (fenvs (mapcar func
		       (butlast xs)
		       (butlast ys)
		       (cdr xs)
		       (cdr ys))))
    (make-fenv
    #'(lambda (x)
	(let ((pos (position x (cdr xs) :test #'<=)))
	  (y (nth pos fenvs) x))))))

(defun mk-linear-fenv (points)
  "Returns a fenv which interpolates the given points by a linear function. Expects a list of x-y-pairs as (0 y1) ... (1 yn)"
  (points->fenv #'(lambda (x1 y1 x2 y2)
			(make-fenv1 #'(lambda (x)
					    (+ (* (- y2 y1) x) y1))
					:min x1 :max x2))
		    points))

(defun mk-sin-fenv (points)
  "Returns a fenv which interpolates the given points by a sin function. Using only the intervals [0,pi/2] and [pi, 3pi/4] results in edges. Expects a list of x-y-pairs as (0 y1) ... (1 yn)."
  (points->fenv #'(lambda (x1 y1 x2 y2)
			(make-fenv1
			 #'(lambda (x)
			     (+ (* (sin (* x pi 0.5)) 
				   (- y2 y1))
				y1))
			 :min x1 :max x2))
		    points))

(defun mk-sin-fenv1 (points)
  "Returns a fenv which interpolates the given points by a sin function without clear edges. Expects a list of x-y-pairs as (0 y1) ... (1 yn)."
  (points->fenv #'(lambda (x1 y1 x2 y2)
			(make-fenv1
			 #'(lambda (x)
			     (+ (* (+ (* (sin (- (* x pi) (* pi 0.5)))
					 0.5)
				      0.5)
				   (- y2 y1))
				y1))
			 :min x1 :max x2))
		    points))

(defun mk-expon-fenv-fn (points)
  "Returns a fenv described be exponential functions. Expects a list of x-y-pairs as (0 y1) ... (1 yn). y values can not be negative.

BUG: Definition wrong -- slope completely bogus!"
  (points->fenv #'(lambda (x1 x2 y1 y2)
			(make-fenv1 #'(lambda (x)
					    (+ (expt (/ y2 y1) x) y1 -1))
					:min x1 :max x2))
		    points))


(flet ((aux (fenvs points)
	    ;; points: (0 ... 1)
	    (make-fenv
	     #'(lambda (x)
		 (let* ((pos (position x points :test #'<))
			(fenv (if pos
			  (make-fenv1 (fenv (nth (1- pos) fenvs))
					  :min (nth (1- pos) points)
					  :max (nth pos points))
			  (first (last fenvs)))))
		   (y fenv x))))))

  (defun fenv-seq (&rest funcenvs-and-points)
    "Combines an arbitrary number of fenvs to a single fenv. Expects its args of form &rest func num func num ... func. The numbers between the funcenvs specify the start resp. end point of a certain fenv. All numbers should be between 0--1 (exclusive)."
    (let ((points (append		; 0, <vals>, 1
		    (at-even-position
		     (cons 0 funcenvs-and-points)) (list 1))) 
	  (fenvs (at-even-position funcenvs-and-points)))
      (aux fenvs points)))
  
  (defun funcs->fenv (funcs &key (min 0) (max 1))
    "Converts a list of unary numeric functions to a single fenv. Max and min a given for all functions and the functions are equally spaced in the func fenv."
    (let ((points (loop for n from 0 to 1 by (/ 1 (length funcs))
			collect n))
	  (fenvs (mapcar #'(lambda (f)
				 (make-fenv f :min min :max max))
			     funcs)))
      ;;(format T "~%debug: points:~A fenvs ~A" points fenvs)
      (aux fenvs points)))

  (defun osciallator (fenv n)
    "Defines a fenv by repeating fenv n times."
    (let ((fenvs (make-list n :initial-element fenv))
	  (points (loop for i from 0 to 1 by (/ 1 n)
			collect i)))
      (aux fenvs points))))

(defun saw-fenv (n &key (amplitude 1) (offset 0))
  "Defines an fenv of saw shape (ascending) with n periods."
  (scale-fenv
   (osciallator (make-fenv #'(lambda (x)
				   (- (* 2 x) 1)))
		n)
   amplitude offset))

(defun saw1-fenv (n &key (amplitude 1) (offset 0))
  "Defines an fenv of saw shape (descending) with n periods."
  (reverse-fenv (saw n :amplitude amplitude :offset offset)))

(defun triangle-fenv (n &key (amplitude 1) (offset 0))
  (scale-fenv
   (osciallator (linear-fenv '((0 -1) (0.5 1) (1 -1))) n)
   amplitude offset))

(defun square-fenv (n &key (amplitude 1) (offset 0))
  (scale-fenv
   (osciallator (make-fenv #'(lambda (x)
				   (if (< x 0.5)
				       -1
				     1)))
		n)
   amplitude offset))

;; irregular-steps (number max min)

(defun steps-fenv (&rest numbers)
  "Outputs a fenv composed of constant functions defined by numbers."
  (funcs->fenv (mapcar #'(lambda (x) #'(lambda (ignore) x))
			   numbers)))

(defun random-steps-fenv (n &key (min-y 0.0) (max-y 1.0))
  (assert (<= min-y max-y))
  (apply #'steps
	 (loop repeat n
	       collect (+ (random (abs (* (- max-y min-y)
					  1.0)))
			  min-y))))	       

(defun rising-expon-fenv (shape)
  "Outputs an rising fenv with exponential shape. y in interval (0, 1]. The bigger shape is the steeper the fenv is. shape must be > 0."
  (assert (> shape 0))
  (make-fenv #'(lambda (x) (exp x)) :min (- shape) :max 0))

(defun constant-fenv (value)
  (make-fenv #'(lambda (x) value)))


;;;
;;; transformers
;;;

(defun combine-fenvs (combi-func &rest fenvs)
  "Returns a fenv which combines the given fenvs with an n-ary numeric function. fenvs can consist of fenvs and numeric values (i.e. constant functions) in any order. The combine-func must except as much numeric values as arguments to fenv are given."
  (make-fenv
   #'(lambda (x)
       (apply combi-func
	      (mapcar
	       #'(lambda (fenv)
		   (cond ((fenv? fenv) (y fenv x))
			 ((numberp fenv) fenv)
			 (T (error "~A is neither a fenv nor a number"
				   fenv))))
	       fenvs)))))

(defun add-fenvs (&rest fenvs)
  "Returns a fenv which adds all given fenvs. fenvs can consist of fenvs and numeric values (i.e. constant functions) in any order."
  (apply #'combine-fenvs (cons #'+ fenvs)))

(defun multiply-fenvs (&rest fenvs)
  "Returns a fenv which adds all given fenvs. fenvs can consist of fenvs and numeric values (i.e. constant functions) in any order."
  (apply #'combine-fenvs (cons #'* fenvs)))



(defun reverse-fenv (fenv)
  (make-fenv
   #'(lambda (x)
       (y fenv (- 1 x)))))


;(defun inverse-fenv (fenv &optional (axis (funcall fenv 0)))
;  )

(defun scale-fenv (fenv mul add)
  (combine-fenvs #'+ add
		(combine-fenvs #'* mul fenv)))


(defun rescale-fenv (fenv &key oldmin oldmax newmin newmax)
  ;; see CM 2.3.4 definition of rescale
  (combine-fenvs
   #'+
   (combine-fenvs #'* (combine-fenvs #'/ (combine-fenvs #'- newmax newmin)
				   (combine-fenvs #'- oldmax oldmin))
		 (combine-fenvs #'- fenv oldmin))
   newmin))

; (plot (rescale-fenv (make-fenv #'sin :max (* pi 2)) :oldmin -1 :oldmax 1 :newmin 0 :newmax 0.5))

;; noise...

;; hp-filter (fenv)
;; lp-filter (fenv)


(defun waveshape (fenv1 fenv2)
  "Returns an fenv which reads fenv1 'through' fenv2: the y value of fenv2 at a given x value access the y of fenv1 (take care to keep output of fenv2 in interval [0,1])."
  (make-fenv
   #'(lambda (x)
       (y fenv1 (y fenv2 x)))))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; utils:
;;;

(defun at-even-position (in-list)
  (at-position in-list 2 0))


;;; EOF
