(in-package :cl)

(defpackage :utilities
  (:use :cl))

(defpackage :validation
  (:use :cl)
  (:export :validate-object))

(defpackage :mutate
  (:use :cl)
  (:export :update-object))

(defpackage :ocflcl
  (:use :cl)
  (:export :main))

(load "ocflcl.lisp")


