(in-package #:cl-plantify)

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
  (delete-duplicates 
   (let ((classes (get-all-symbols #'classp package)))
     (loop for class in classes
	collecting (list :name class
			 :slots (get-slots class)
			 :subclasses (find-subclasses class))))
   :key (lambda (class) (getf class :name))))

(defun classes-to-plantuml (classes &optional (stream t))
  (mapcar (lambda (class)
	    (format stream "class \"~a\"{ ~%~{ + ~S ~%~}}~%" (getf class :name) (getf class :slots))) classes)

  (mapcar (lambda (class)

	    (let ((quantity (length (getf class :subclasses))) 
		  lengths)
	      (cond  
		((< quantity 5) (setq lengths '("--")))
		((< quantity 10) (setq lengths '("--" "---")))
		((< quantity 20) (setq lengths '("--" "---" "----")))
		(t (setq lengths '("--" "---" "----" "-----" "------"  "-------"  "--------"  "---------"))))

	      (mapcar (lambda (subclass)
			(format stream "\"~a\" <|~a \"~a\"~%" (getf class :name) (car lengths) subclass)
			(setf lengths (append (rest lengths) (list (first lengths)))))
		      (getf class :subclasses))))
	  classes)
  t)

(defun classes-to-plantuml-with-packages (classes &optional (stream t))
  (macrolet ((my-get-hash (class hash)
	     `(gethash (symbol-package (getf ,class :name)) ,hash)))
    (let ((packages (make-hash-table)))
      (mapcar (lambda (class)
		(setf (my-get-hash class packages) 
		      (cons class (my-get-hash class packages))))
	      classes)
      (loop for package being the hash-keys of packages
	 using (hash-value classes)
	 do
	   (format stream "package \"~a\" {~%" (package-name package))
	   (classes-to-plantuml classes stream)
	   (format stream "}~%")))))


;; (with-open-file (archivo "/home/lucas/quicklisp/local-projects/cl-plantify/cffi.plant" :if-exists :supersede :direction :output)
;;   (format archivo ";;; -*- Mode: org; -*- ~%#+begin_src plantuml :file cffi.svg~%")
;;   (classes-to-plantuml-with-packages (reduce #'append
;; 					     (mapcar #'get-all-classes
;; 						     (delete (package-name 'common-lisp)
;; 							     (append (mapcar #'package-name (package-use-list (find-package 'ql))) '(ql) ))))
;; 				     archivo)
;;   (format archivo "#+end_src"))
