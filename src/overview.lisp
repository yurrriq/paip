(in-package #:paip)
(defpackage #:paip.overview
  (:use #:cl #:alexandria #:check-it #:clunit)
  (:shadowing-import-from #:alexandria #:set-equal))
(in-package #:paip.overview)

(defun length-r (lst)
  (reduce #'+ lst :key (constantly 1)))

(defsuite overview-suite ())

(register-package-regression-file :paip.overview "regressions.lisp")

(deftest test-length-r-check-it (overview-suite)
  (let ((lst (iota 4)))
    (assert-equal (length lst) (length-r lst))))

(deftest test-length-r-check-it (overview-suite)
  (let ((*num-trials* 100))
    (assert-true
      (check-it (generator (list (integer)))
                (lambda (lst)
                  (assert-equal (length lst) (length-r lst)))))))
