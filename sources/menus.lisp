(in-package :fe)

;; define a user menu
(add-PWGL-user-menu 
 '(:menu-component
   ("Fenv"
    (("Accessing fenv data" (; fenv->BPF 
			     fenv->list y fenv?))
     ("Concrete generators" (BPF->fenv mk-linear-fenv mk-sin-fenv mk-sin-fenv1
			     saw-fenv saw1-fenv triangle-fenv square-fenv
			     steps-fenv random-steps-fenv rising-expon-fenv constant-fenv))
     ("Generic generators" (make-fenv make-fenv1 
			    ; points->fenv ; likely too complicated for general users
			    funcs->fenv fenv-seq osciallator))
     ("Concrete transformers" (add-fenvs multiply-fenvs reverse-fenv scale-fenv rescale-fenv))
     ("Generic transformers" (combine-fenvs waveshape))
     ))))

