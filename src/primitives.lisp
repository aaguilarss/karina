; This file is a part of Senna
; (C) 2020 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/primitives
  (:use :cl :senna/toolkit)
  (:export :car! :cdr! :cons!)
  (:export :list-model-car :list-model-cdr
           :list-model-cons)
  (:export :find-list-model :define-model
           :default)
  (:export :senna :in-model :with-model))
(in-package :senna/primitives)


;; List Models
(defparameter *list-models* (make-hash-table)
  "List model storage")

(defparameter *list-model* nil
  "The current list model")


(defclass list-model ()
  ((car
     :initarg :car
     :accessor list-model-car
     :type function)
   (cdr
     :initarg :cdr
     :accessor list-model-cdr
     :type function)
   (cons
     :initarg :cons
     :accessor list-model-cons
     :type function)))


(defun find-list-model (s)
  (gethash (sy! s) *list-models*))

(defun (setf find-list-model) (new s)
  (setf (gethash (sy! s) *list-models*) new))

(defmacro define-model (name &key car cdr cons)
  `(setf (find-list-model ',name)
         (make-instance 'list-model
                        :car ,car
                        :cdr ,cdr
                        :cons ,cons)))


;; The built-in model. Offers default behaviour for lists
;; and to add to the model one can add methods to the generic
;; functions.

(defgeneric car! (x)
  (:documentation "The abstract version of car"))

(defgeneric (setf car!) (new x)
  (:documentation "The abstract version of (setf car)"))

(defmethod car! ((x list))
  (car x))


(defgeneric cdr! (x)
  (:documentation "The abstract version of cdr"))

(defgeneric (setf cdr!) (new x)
  (:documentation "The abstract version of (setf cdr)"))

(defmethod cdr! ((x list))
  (cdr x))


(defgeneric cons! (x y)
  (:documentation "An abstract  version of cons"))

(defmethod cons! (x (y list))
  (cons x y))


(define-model default
  :car (lambda (x) (funcall #'car! x))
  :cdr (lambda (x) (funcall #'cdr! x))
  :cons (lambda (x y) (funcall #'cons! x y)))



;; Senna macro

(defmacro car!% (x)
  `(funcall (list-model-car *list-model*) ,x))

(defmacro cdr!% (x)
  `(funcall (list-model-cdr *list-model*) ,x))

(defmacro cons!% (x y)
  `(funcall (list-model-cons *list-model*) ,x ,y))


(defmacro senna (&body body)
  `(progn
     ,@(subst 'car!% 'car
         (subst 'cdr!% 'cdr
           (subst 'cons!% 'cons body)))))


;; in-model macro
(defmacro in-model (x)
  "Make model x current"
  `(setf *list-model* (find-list-model ',x))) 

(in-model :default) ; Set default list model
 

;; with-model macro
(defmacro with-model (x &body body)
  "Evaluate forms of body with list-model x"
  `(let ((*list-model* (find-list-model ',x)))
     ,@body))
