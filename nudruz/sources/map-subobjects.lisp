(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defmethod map-subobjects (fn container &key key recurse test type)
  (let ((test (if type (lambda (x) (typep x type)) test)))
    (if key
        (if test
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (when (funcall test o)
                    (funcall fn (funcall key o)))))
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (funcall fn (funcall key o)))))
        (if test
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (when (funcall test o) (funcall fn o))))
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (funcall fn o)))))
    (values)))

(defmethod map-subcontainers (fn container &key key recurse)
  (if key
      (dolist (o (subobjects container))
        (when (typep o <container>) (funcall fn (funcall key o)))
        (if recurse
            (map-subcontainers fn o :key key :recurse recurse)))
      (dolist (o (subobjects container))
        (when (typep o <container>) (funcall fn o))
        (if recurse
            (map-subcontainers fn o :key key :recurse recurse))))
  (values))

