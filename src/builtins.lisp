; This file is a part of Senna
; (C) 2019 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/builtins
  (:use :cl :senna/primitives))

(in-package :senna/builtins)


; This one is justified because (typep (cons 1 2) 'list) returns T

(defmethod car! ((obj list))
  (car obj))

(defmethod (setf car!) (new (obj list))
  (setf (car obj) new))


(defmethod cdr! ((obj list))
  (cdr obj))

(defmethod (setf cdr!) (new (obj list))
  (setf (car obj) new))


(defmethod cons! (obj1 (obj2 list))
  (cons obj1 obj2))
