;;
;; tap.lisp - Test Anything Protocol support for CL
;;

(defvar *test-number* 1)
(defvar *test-prefix* "")

(defun should (msg test)
  (cond (test (format t "~&ok ~d - ~a~a~%" *test-number* *test-prefix* msg))
        (t (format t "~&not ok ~d - ~a~a~%" *test-number* *test-prefix* msg)))
  (incf *test-number*))

(defmacro diag (fmt &rest args)
  `(progn
    (format t "#")
    (format t ,fmt ,@args)))

(defun should-equal (msg got want)
  (should msg (equal got want))
  (cond ((not (equal got want))
         (diag "got    ~S~%" got)
         (diag "wanted ~S~%" want))))

(defun plan (n)
  (format t "1..~d~%" n))

(defun done-testing ()
  (format t "1..~d~%" (- *test-number* 1)))
