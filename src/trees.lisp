; This file is a part of Senna
; (C) 2019 - Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/trees
  (:use :cl :senna/primitives))

(in-package :senna/trees)

(defun tree-equal! (tree-1 tree-2 &key test) 
  "Returns T if tree-1 and tree-2 have the same")
