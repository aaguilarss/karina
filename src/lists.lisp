; This file is a part of Senna
; (C) Abraham Aguilar a.aguilar@ciencias.unam.mx

(uiop:define-package :senna/lists
  (:use :cl :senna/primitives)
  (:import-from :let-over-lambda :defmacro!)
  (:export :pop! :push! :with-iterator :mapcar!
           :length! :last! :nth! :append! :dolist!
           :nthcdr! :member! :butlast! :nbutlast!
           :rplaca! :rplacd! :adjoin! :pushnew!
           :length!))

(in-package :senna/lists)

(defun member! (obj list &key (test #'eql) (key #'identity))
  "Returns the tail of list starting with obj if obj is a member of
  list, or return Nil if obj is not in list"
  (when list
    (if (funcall test (funcall key (car! list)) obj)
      list
      (member obj (cdr! list)))))

(defun nthcdr! (n list)
  "Return tail of list after calling cdr n times"
  (if (= 0 n)
    list
    (nthcdr! (1- n) list)))

(defun nth! (n list)
  "Zero-indexed nth element of list"
  (if (= n 0)
    (car list)
    (nth! (1- n) (cdr list))))

(defun last! (list)
  "Return last cdr! of list"
  (if (null (cdr! list))
    list
    (last (cdr! list))))

(defun butlast! (list)
  "Return list excluding the last cons"
  (if (null (cdr! list))
    nil
    (cons (car! list) (butlast! (cdr! list)))))

(defun nbutlast! (n list)
  "Return list excluding the last n conses"
  (if (= n 0)
    list
    (nbutlast! (1- n) (butlast! list))))

(defun rplaca! (list obj)
  "Replace car of list with object"
  (cons! obj (cdr! list)))

(defun rplacd! (list obj)
  "Replace cdr of list with object"
  (cons! (car! list) obj))

(defun adjoin! (obj list &key (test #'eql) (key #'identity))
  "Return list if obj is a member of list, or return (cons obj list) if
  it isnt"
  (if (member! obj list :test test :key key) list (cons! obj list)))

(defmacro pop! (obj)
  "Set place named obj to (cdr! obj), return (car! obj)"
  `(let ((x (car! ,obj)))
     (setf ,obj (cdr! ,obj))
     x))

(defmacro push! (obj place)
  "Set place to (cons! obj place)"
  `(setf place (cons! ,obj ,place)))

(defmacro pushnew! (obj place &key (test #'eql) (key #'identity))
  "Set place to (adjoin obj place :test test :key key"
  `(setf place (adjoin ,obj ,place, :test ,test :key ,key)))

(defun append! (&rest lists)
  "Return concatenated lists"
  (if (null (car lists))
    (when (cdr lists)
      (apply #'append! (cdr lists)))
    (cons! (car! (car lists)) (apply #'append! (cons (cdr! (car lists)) (cdr lists))))))

(defun length! (list)
  "Return length of list"
  (if (null list)
    0
    (1+ (length! (cdr! list)))))

(defun mapcar! (function list &rest more)
  "Return list of values of function applied succesively to cars of lists"
  (loop for args = (loop for i in (cons list more) while (car i) collect (car! i))
        while (= (length args) (length (cons list more)))
        collect
        (apply function args)
        do
        (setf list (cdr list))
        (setf more (loop for i in more collecting (cdr i)))))

(defmacro! with-iterator ((iterator-name place) &body forms)
  "Return values of forms. In forms, successive invocations of (iterator-name)
    return sucessive members of place"
  `(labels ((,g!reading-closure (let ((a ,place))
                                  (lambda () (pop! a))))
            (,iterator-name () (funcall ,g!reading-closure)))
     ,@forms))

(defmacro! dolist! ((var list) &body forms)
  "Evaluate forms in a tagbody with var succesively bound to the elements of list"
  `(block nil
      (let ((,g!list ,list))
        (tagbody
          ,g!start
          (unless (endp ,g!list)
            (let ((,var (car! ,g!list)))
              (setq ,g!list (cdr! ,g!list))
              (tagbody ,@forms))
            (go ,g!start))))
      nil))
