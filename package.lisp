(in-package :cl)

(defpackage :utils
  (:use :cl)
  (:export :if-let :add-default-key :subtract-set :relative-path :hex-of-file))

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


