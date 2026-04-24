(in-package :validation)

; Validation:
; - [ ] Created needs to conform to RCFS Standards
; - [ ] Address in the user
; - [ ] pathnames _must_ be relative
; - [ ] type must be an iri
; - [ ] got a heap error while validating b22b9gr8mx0m also: 61001/b2343579qm2c *on* BSD, it works on Linux.
;
;
(ql:quickload '(:com.inuoe.jzon :ironclad))
(sb-ext:add-package-local-nickname :jzon :com.inuoe.jzon)
(load "utils.lisp")

(defun test-hashes (manifest-map  path-base inventory-set &optional (is-valid t))
  (declare (type hash-table manifest-map)
           (type boolean is-valid )
           (type hash-table inventory-set)
           (type pathname path-base))
  "take the manifest and test each of the files in the manifest against the key
  the path base is going to be the base for the files listed in the manifest.
  For the base level, this will be the root directory of the object, for the versions,
  this will be the content directory for each version
  the `is-valid` option will default to true, when iteratirng through the object tree, this value
  can be passed to other calls of this function"
  (maphash
    (lambda (key val)
      (loop
        for file
        across val
        do (let* ((filepath (pathname (merge-pathnames file path-base)))
                  (file-hash (utils:hex-of-file :sha512 filepath)))
             (setf (gethash filepath inventory-set) t)
             (unless (string-equal file-hash key)
              (progn
                (setf is-valid nil)
                (write-line (format nil "Error with file: ~A~%Expected: ~A~%   Found: ~A~%" filepath key file-hash)))))))
      manifest-map)
  is-valid)

(defun update-file-set (file-set base-dir )
  (declare (type hash-table file-set)
           (type string base-dir))
  "Creates a set of all of the files in the object"
  (loop
    for f in (directory (merge-pathnames "**/*.*" base-dir))
    do (if (pathname-name (probe-file f))
         (let ((file-fullname (file-namestring f)))
          (unless (find file-fullname '("inventory.json" "0=ocfl_object_1.0" "0=ocfl_object_1.1" "inventory.json.sha512") :test 'string-equal)
              (setf (gethash f file-set) t)))))
  file-set)

(defun extract-stored-inv-hash (inventory-file)
  (declare (type pathname inventory-file))
  ""
  (let* ((inventory-hash-file (pathname (concatenate 'string (namestring inventory-file) ".sha512")))
         (lines (with-open-file (stream inventory-hash-file) (uiop:slurp-stream-lines stream)))
         (inventory-hash (car lines)))
    (car ( split-sequence:split-sequence #\Space  inventory-hash))))

(defun test-inventories (base-dir &optional (is-valid t))
  (declare (type string base-dir)
           (type boolean is-valid))
  "find all of the inventory files in the directory, look specifically at the .sha512 file"
  (loop
    for inventory-file
    in (directory (merge-pathnames "**/inventory.json" base-dir))
    do (let ((found-hash (utils:hex-of-file :sha512 inventory-file))
             (expected-hash (extract-stored-inv-hash inventory-file)))
         (unless (string-equal found-hash expected-hash)
           (progn
            (setf is-valid nil)
            (write-line (format nil "Error with inventory: ~A~%Expected: ~A~%   Found: ~A~%" inventory-file expected-hash found-hash))))))
  is-valid)

(defun update-manifest-set (manifest-set base-dir &optional (print-warnings nil))
  (declare (type hash-table manifest-set)
           (type pathname base-dir)
           (type boolean print-warnings))
  "Updates a set that contains all of the sets inside of the manifests
  This will iterate through all of the inventory.json files, and will double entry many
  of these entries.
  "
  (let ((is-valid t))
    (loop
      for inventory-file
      in (directory (merge-pathnames "**/inventory.json" base-dir))
      do (let ((inventory (jzon:parse inventory-file)))
          (progn
            (test-hashes (gethash "manifest" inventory) base-dir manifest-set is-valid)
            (maphash
              (lambda (version object)
                (let ((version-dir (merge-pathnames (concatenate 'string version "/content/" ) base-dir)))
                  (when print-warnings (loop
                    for key in '("created" "message" "user")
                    unless (gethash key object)
                    do (format t "WARNING: ~A not in inventory: ~A~%" key inventory-file)
                    ))
                  (test-hashes (gethash "state" object) version-dir manifest-set is-valid)))
              (gethash "versions" inventory)))))))

(defun validate-object (base-dir &optional (print-warnings nil))
  (declare (type string base-dir))
  ""
  (let* ((manifest-set (make-hash-table :test 'equal))
        (file-set (make-hash-table :test 'equal))
        (is-valid t))
    (progn
      (setf is-valid (update-manifest-set manifest-set (pathname base-dir) print-warnings))
      (update-file-set file-set base-dir)
      (setf is-valid (test-inventories base-dir))
      is-valid)))


; These  are used primarily for testing
;(defparameter *my-file-set* (make-hash-table :test 'equal))
;(defparameter *my-manifest-set* (make-hash-table :test 'equal))

;(update-manifest-set *my-manifest-set* (pathname "testdata/8p/"))
;(update-file-set *my-file-set* "testdata/8p/")
;(let (
;      (base-dir "testdata/8p/")
;      (manifest-set (make-hash-table :test 'equal))
;      (file-set (make-hash-table :test 'equal)))
;  (progn
;    (update-file-set file-set base-dir)
;    (update-manifest-set manifest-set (merge-pathnames (pathname base-dir) (truename ".")))
;    (loop
;      for key being the hash-keys of (utils:subtract-set file-set manifest-set)
;      do (format t "unexpected file found: ~A~%" key))))


