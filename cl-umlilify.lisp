;;;; cl-umlilify.lisp

(in-package #:cl-umlilify)

;;; "cl-umlilify" goes here. Hacks and glory await!

(defun get-all-symbols (test &optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (funcall test  s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))

(defun classp (symbol)
  (closer-mop:classp (find-class symbol nil)))

(defun find-subclasses (class)
  (mapcar #'class-name (closer-mop:class-direct-subclasses (find-class class))))

(defun get-slots (class)
  (handler-case (mapcar #'closer-mop:slot-definition-name 
			(closer-mop:class-slots (find-class class)))
    ;;swank::channel signals 'sb:reference-error' 
    (error () nil)))

(defun get-all-classes (&optional (package nil))
  (let ((classes (get-all-symbols #'classp package)))
    (loop for class in classes
	 collecting (list :name class
			  :slots (get-slots class)
			  :subclasses (find-subclasses class)))))

(defun classes-to-plantuml (classes &optional (stream t))
  (mapcar (lambda (class)
	    (format stream "class \"~a\"{ ~%~{ - ~a ~%~}}~%" (getf class :name) (getf class :slots))) classes)
  (mapcar (lambda (class)
	    (mapcar (lambda (subclass)
		      (format stream "\"~a\" <|--- \"~a\"~%" (getf class :name) subclass))
		    (getf class :subclasses)))
	  classes)
  t)

(with-open-file (archivo "/home/lucas/quicklisp/local-projects/cl-umlilify/diagrama.plant" :if-exists :supersede :direction :output)
  (classes-to-plantuml (get-all-classes t) archivo))


