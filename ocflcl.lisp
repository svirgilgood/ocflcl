(in-package :ocflcl)

(ql:quickload "clingon")
(ql:quickload "local-time")
(load "validation.lisp")
(load "mutate.lisp")


(defun mutate/options ()
  (list
    (clingon:make-option
      :string
      :description "user name who created the object"
      :short-name #\u
      :long-name "user"
      :key :user)
    (clingon:make-option
      :string
      :description "email address"
      :short-name #\a
      :long-name "address"
      :key :email)
    (clingon:make-option
      :string
      :description "message"
      :short-name #\m
      :long-name "message"
      :key :message)
    (clingon:make-option
      :string
      :description "path to ocfl object"
      :short-name #\o
      :long-name "ocfl"
      :key :ocfl)
    (clingon:make-option
      :string
      :description "timestamp where the object was created"
      :short-name #\c
      :long-name "created"
      :key :created)))

(defun mutate/handler (cmd)
  (let ((ocfl (clingon:getopt cmd :ocfl))
        (user (clingon:getopt cmd  :user))
        (email (clingon:getopt cmd :email))
        (created (clingon:getopt cmd :created))
        (message (clingon:getopt cmd :message))
        (user-hash (make-hash-table :test 'equal)))
    (if user
        (progn
          (setf (gethash "name" user-hash) user)
          (setf (gethash "address" user-hash) email))
        (setf user-hash nil))

    (mutate:update-object (pathname ocfl) :user user-hash :created created :message message)))
    ;(loop
    ;  for ocfl in ocfls
    ;  do (progn
    ;       (write-line ocfl)
    ;       (update:update-object (pathname ocfl) :user user-hash :created created :message message )))))
(defun mutate/command ()
  ""
  (clingon:make-command
    :name "mutate"
    :description "mutate an existing ocfl object so that it is conforming"
    :version "0.1.1"
    :options (mutate/options)
    :handler #'mutate/handler))


(defun validate/options ()
  (list
    (clingon:make-option
      :string
      :description "path to the ocfl object"
      :short-name #\o
      :long-name "ocfl"
      :key :ocfl)
    (clingon:make-option
      :boolean
      :description "print the warnings for the object"
      :short-name #\w
      :long-name "warnings"
      :key :warnings)))

(defun validate/handler (cmd)
  (let ((ocfl (clingon:getopt cmd :ocfl))
        (warnings (clingon:getopt cmd :warnings)))
    (validation:validate-object ocfl warnings)))

(defun validate/command ()
  (clingon:make-command
    :name "validate"
    :description "validate a specific ocfl objects"
    :version "0.0.4"
    :options (validate/options)
    :handler #'validate/handler))

(defun top-level/sub-commands ()
  (list
    (mutate/command)
    (validate/command)))

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command
    :name "ocflcl"
    :version "0.0.2"
    :description "A general purpose OCFL Command line app"
    :authors '("svirgilgood https://github.com/svirgilgood")
    :license "BSD 2-Clause"
    :handler #'top-level/handler
    :sub-commands (top-level/sub-commands)))

(defun main ()
  ""
  (let ((app (top-level/command)))
    (clingon:run app )))


