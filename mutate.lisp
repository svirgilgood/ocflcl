(in-package :mutate)
(ql:quickload '(:com.inuoe.jzon :ironclad :alexandria))
(sb-ext:add-package-local-nickname :jzon :com.inuoe.jzon)

; utility functions, these need to be pulled into their own package
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

(defun get-content-file-name (path)
  (declare (type pathname path))
  "Return"
  (let ((parent (first (last (pathname-directory path))))
        (file-name (file-namestring path)))
    (unless (string-equal parent "content")
        (setf file-name (concatenate 'string parent "/" file-name)))
    file-name))

; update application functions
(defun create-version-inventory (version-path head-hash-table &key digest-algorithm created message user)
  (declare (type pathname version-path)
           (type hash-table head-hash-table)
           (type keyword digest-algorithm)
           (type (or null string) created message)
           (type (or null hash-table) user))
  "Create the version inventory for the given path
  This function is not about parsing the inventory file, only reading the inventory of the
  file system.
  The digest-algorithm is required because it should come from the base of the object
  "
  (let* ((version-inventory (jzon:parse (merge-pathnames "inventory.json" version-path)))
         (new-version-inventory (make-hash-table :test 'equal))
         (version-name (first (last (pathname-directory version-path))))
         (old-version (gethash version-name (gethash "versions" version-inventory))))
    (add-default-key created new-version-inventory old-version)
    (add-default-key message new-version-inventory old-version)
    (add-default-key user new-version-inventory old-version)
    (loop
      for file in (directory (merge-pathnames "content/**/**.*" version-path))
      with state-hash-table = (make-hash-table :test 'equal)
      when (pathname-name (probe-file file) )
      do (let ((file-hash (hex-of-file file :digest-algorithm digest-algorithm))
               (file-name (get-content-file-name file))
               (file-relative-path  (merge-pathnames file version-name))
               (file-metadata (make-hash-table :test 'equal)))
           ; if parent of file does not equal content, parent needs to be added to file name
           (setf (gethash file-hash state-hash-table) (vector file-name ))
           (setf (gethash "hash" file-metadata) file-hash)
           (setf (gethash "full-path" file-metadata) file-relative-path)
           (setf (gethash file-name head-hash-table) file-metadata))
      finally (setf (gethash  "state" new-version-inventory ) state-hash-table))
    new-version-inventory))

;(jzon:stringify (create-version-inventory (pathname "./testdata/b2/1k/00/99/ck/99/v1/") *head-hash-table* :digest-algorithm :sha512 :created "2026-04-20T04:20Z" :message nil) :pretty t)
;
(defun write-inventory-hash (inventory-path &key ( digest-algorithm :sha512) )
  (declare (type pathname inventory-path)
           (type keyword digest-algorithm))
  (let* ((hash (hex-of-file inventory-path :digest-algorithm digest-algorithm ))
        (hash-path (pathname (concatenate 'string (namestring inventory-path) "." (string-downcase (string digest-algorithm))))))
    (with-open-file (stream hash-path :direction :output :if-exists :overwrite)
      (format stream "~A inventory.json" hash))))

(defun update-object (object-path &key digest-algorithm ocfl-type created message user)
  (declare (type pathname object-path)
           (type (or null keyword) digest-algorithm)
           (type (or null string) created message ocfl-type)
           (type (or null hash-table) user))
  "iterate through the version directories
  Create a version inventory, update the head-hash-table
  write the version inventory
  hash the version inventory
  create the object inventory
  write the object inventory
  hash the object inventory
  the head version inventory is going to be the same as the root version inventory
  "
  (let* ((old-inventory (jzon:parse (merge-pathnames "inventory.json" object-path)))
        (head-hash-table (make-hash-table :test 'equal))
        (new-inventory (make-hash-table :test 'equal))
        (versions (make-hash-table :test 'equal))
        (id (gethash "id" old-inventory))
        (derived-ocfl-type (gethash "type" old-inventory)))
    (unless digest-algorithm
      (let ((da (gethash "digestAlgorithm" old-inventory)))
        (setf (gethash "digestAlgorithm" new-inventory) da)
        (setf digest-algorithm (intern (string-upcase da) :keyword))))
    ;(if ocfl-type
    ;    (progn (setf (gethash "type" new-inventory) ocfl-type) (write-line ocfl-type))
    ;    (progn (setf (gethash "type" new-inventory) derived-ocfl-type) (write-line derived-ocfl-type)))

    (setf (gethash "id" new-inventory) id)
    (setf (gethash "versions" new-inventory) (make-hash-table :test 'equal))

    (loop
      for version-path in (directory (merge-pathnames "*" object-path))
      do (let* ((version-inventory (create-version-inventory version-path head-hash-table :digest-algorithm digest-algorithm :created created :message message :user user))
               (version-name (first (last (pathname-directory version-path))))
               (version-inventory-path (merge-pathnames (concatenate 'string version-name "/inventory.json") object-path)))
           (setf (gethash version-name versions) version-inventory)
           (setf (gethash "head" new-inventory) version-name)
           (setf (gethash "manifest" new-inventory) (make-hash-table :test 'equal))
           (if ocfl-type
              (setf (gethash "type" new-inventory) ocfl-type)
              (setf (gethash "type" new-inventory) derived-ocfl-type))
           (maphash
             (lambda (_ value)
               (declare (ignorable _))
               (let ((hash-value (gethash "hash" value))
                     (file-path (relative-path (pathname (gethash "full-path" value)) object-path)))
                 (setf (gethash hash-value (gethash "manifest" new-inventory)) (vector file-path))))
             head-hash-table)

           (setf (gethash version-name (gethash "versions" new-inventory)) version-inventory)
           (jzon:stringify new-inventory :pretty t :stream version-inventory-path)
           (write-inventory-hash version-inventory-path :digest-algorithm digest-algorithm))
      finally (let ((root-inventory (merge-pathnames "inventory.json" object-path))
                    (head (gethash "head" new-inventory)))
                ;(jzon:stringify new-inventory :pretty t :stream root-inventory)
                (uiop:copy-file (merge-pathnames (concatenate 'string head "/inventory.json") object-path) root-inventory)
                (write-inventory-hash root-inventory :digest-algorithm digest-algorithm)))))





