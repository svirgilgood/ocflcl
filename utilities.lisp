; utility functions, these need to be pulled into their own package
(in-package :utilities)


(defmacro if-let (( var test-form ) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))

(defmacro add-default-key (key new-hash-table old-hash-table)
  (let ((key-string (string-downcase (string key))))
    (alexandria:once-only (key-string)
      `(if ,key
           (setf (gethash ,key-string ,new-hash-table) ,key)
           (setf (gethash ,key-string ,new-hash-table) (gethash ,key-string ,old-hash-table))))))


(defun subtract-set (seta setb)
  (declare (type hash-table seta)
           (type hash-table setb))
  "Takes seta and if the key is not found in setb, a new set is created"
  (let ((result (make-hash-table :test 'equal)))
    (loop
      for key being the hash-keys of seta
      unless (gethash key setb)
      do     (setf (gethash key result) t))

    result))

(defun relative-path (path base)
  (declare (type pathname path base))
  "Given a base path, return the relative path of the object as a string"
  (enough-namestring (namestring path) (namestring base)))



(defun read-file-to-bytes (path)
  (declare (type pathname path))
  "Read a file path and return an array of bytes from the file"
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun hex-of-file (path &key (digest-algorithm :sha512))
  (declare (type pathname path)
           (type (or null keyword) digest-algorithm))
  "Return the hex string of the file in the path supplied"
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence digest-algorithm (read-file-to-bytes path))))


