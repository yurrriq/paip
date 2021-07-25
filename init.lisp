#-quicklisp
(let ((quicklisp-init "quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push (uiop:getcwd) asdf:*central-registry*)

(asdf:load-system :paip)
