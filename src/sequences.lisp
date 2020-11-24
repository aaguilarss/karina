; This file is a part of Senna
; (C) 2019 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/sequences
  (:use :cl :senna/primitives))

(in-package :senna/sequences)


(defun every! (test sequence)
  (if (null sequence)
    T
    (and (funcall test (car! sequence))
         (every! test (cdr! sequence)))))

(defun notevery! (test sequence)
  (if (null sequence)
    nil
    (or (not (funcall test (car! sequence)))  
        (notevery! test (cdr! sequence)))))

(defun some! (test sequence)
  (if (null sequence)
    nil
    (or (funcall test (car sequence))
        (some! test (cdr sequence)))))

(defun notany! (test sequence)
  (not (some! test sequence)))
