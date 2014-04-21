(ql:quickload :closer-mop)
(defpackage myutil
  (:use :common-lisp)
  (:export :range
	   :nand
	   :nor
	   :while
	   :read-list
	   :mkstr
	   :symb
	   :reread
	   :def-exporting-class
	   :when-bind
	   :if-bind
	   :with-all-slots
	   :mac
	   :read-loop))

(in-package myutil)

(defun range (min &optional (max 0 max-supplied-p) (step 1))
  (if max-supplied-p
      (loop for n from min below max by step collect n)
      (loop for n from 0 below min collect n)))

(defmacro nand (&body body)
  `(not (or ,@body)))

(defmacro nor (&body body)
  `(not (and ,@body)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun concat-symbol (&rest syms)
  (read-from-string (apply #'concatenate 'string (mapcar 'symbol-name syms))))

(defmacro syms-defun (syms &body body) 
  `(defun ,(apply 'concat-symbol syms) ,@body))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn a #'(lambda (x) (> x b)) #'(lambda (x) (+ x step))))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defmacro def-exporting-class (name (&rest superclasses) (&rest slot-specs)
                               &optional class-option)
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name)))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,(append 
           (mapcar (lambda (spec)
                     (let ((export-pos (position :export spec)))
                       (if export-pos
                       (append (subseq spec 0 export-pos)
                           (subseq spec (+ 2 export-pos)))
                       spec)))
               slot-specs)
           (when class-option (list class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro if-bind ((var expr) then else)
  `(let ((,var ,expr))
     (if ,var
	 ,then
	 ,else)))
  

(defmacro with-all-slots ((object class) &body body)
  (let ((slot-list (mapcar
		    #'(lambda (x) (list x x))
		    (mapcar 
		     #'closer-mop:slot-definition-name 
		     (closer-mop:class-slots (find-class class))))))
    `(with-slots ,slot-list ,object ,@body)))	   

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro def-simple-class (name (&rest super-class) (&rest vars))
  `(defclass ,name (,@super-class)
     ,(mapcar #'(lambda (x)
		  `(,x 
		    :initarg ,(read-from-string 
			      (concatenate 'string ":" (symbol-name x)))
		    :initform nil
		    :accessor ,x))
	      vars)))

(defmacro read-loop (var test then else &key (initfn '(read)) (updatefn initfn))
  `(do ((,var ,initfn ,updatefn))
       (,test ,then)
     ,else))