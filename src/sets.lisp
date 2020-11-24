; This file is a part of Senna
; (C) 2019 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/sets
  (:use :cl))

(in-package :senna/sets)


(defun intersection! (a b &key (test #'eql) (key #'identity)))


(defun set-difference! (a b &key (test #'eql) (key #'identity)))


(defun union! (a b &key (test #'eql) (key #'identity)))


(defun set-exclusive-or! (a b &key (test #'eql) (key #'identity)))
