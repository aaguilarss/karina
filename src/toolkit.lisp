; This file is a part of Senna
; (C) 2020 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/toolkit
  (:use :cl)
  (:export :sy! :s!))
(in-package :senna/toolkit)

(defun s! (&rest args)
  "Build string PRINCing args. If at any point in the args
  a string with the char ~ is found, this string is treated
  as a FORMAT control string taking as many arguments from args
  as ~'s are found in the control string"
  (with-output-to-string (s)
    (loop while args doing
          (let ((it (pop args)))
            (typecase it
              (string
                (if (member #\~ (coerce it 'list))
                  (let ((n (count-if
                             (lambda (x) (equal #\~ x))
                             (coerce it 'list))))
                    (apply #'format (l! s it (loop for i from 1 to n collecting
                                                   (pop args)))))
                  (princ it s)))
              (otherwise (princ it s)))))))

(defun sy! (&rest args)
  (values (intern (string-upcase (apply #'s! args)))))
