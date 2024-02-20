(in-package :ac-collisions-mt/cli)

(defparameter *database-directory* #p"~/.cache/ac-collisions/")

(defun print-usage-and-exit ()
  (format *error-output*
"Usage: ac-collisions-mt process degree [-w workers] [-c count]
        ac-collisions-mt show-summary
        ac-collisions-mt show-poly degree
")
  (uiop:quit 1))

(defun parse-integer-or-exit (string)
  (handler-case
      (parse-integer string)
    (sb-int:simple-parse-error () (print-usage-and-exit))))

(defun parse-options (options keymap on-error)
  (labels ((%go (acc options)
             (if (null options) acc
                 (let ((pair (assoc (car options) keymap :test #'string=)))
                   (if (not pair)
                       (funcall on-error)
                       (%go (cons (cons (cdr pair) (cadr options)) acc)
                            (cddr options)))))))
    (%go nil options)))

(defgeneric launch (command &key &allow-other-keys))

(defun parse-process (argv)
  (when (zerop (length argv))
    (print-usage-and-exit))
  (let* ((degree (parse-integer-or-exit (car argv)))
         (options (parse-options (cdr argv)
                                 '(("-w" . :workers) ("-c" . :count))
                                 #'print-usage-and-exit))
         (workers (let ((workers (cdr (assoc :workers options))))
                    (if workers (parse-integer-or-exit workers) 1)))
         (count   (let ((count (cdr (assoc :count options))))
                    (if count (parse-integer-or-exit count)))))
    (launch :process :degree degree :workers workers :count count)))

(defun parse-summary (options)
  (declare (ignore options))
  (launch :summary))

(defun parse-poly (options)
  (unless (= (length options) 1)
    (print-usage-and-exit))
  (launch :show-poly
          :degree (parse-integer-or-exit
                   (first options))))

(defun parse-commands (argv)
  (when (zerop (length argv))
    (print-usage-and-exit))
  (destructuring-bind (command . options) argv
    (funcall
     (cond
       ((string= command "process")
        #'parse-process)
       ((string= command "show-summary")
        #'parse-summary)
       ((string= command "show-poly")
        #'parse-poly)
       (t (lambda (x)
            (declare (ignore x))
            (print-usage-and-exit))))
     options)))

(defun get-db-pathname ()
  (merge-pathnames "polynomials.db" *database-directory*))

(defun shutdown (c)
  (declare (ignore c))
  (format *standard-output* "Stopping worker threads~%")
  (invoke-restart 'stop-workers))

(defmethod launch ((command (eql :summary)) &key &allow-other-keys)
  (let ((summary (summary (get-db-pathname))))
    (dolist (entry summary)
      (apply #'format *standard-output*
             "Degree=~d, Processed=~d, Collisions=~d, Irreducible=~d~%"
             entry))))

(defmethod launch ((command (eql :show-poly)) &key degree &allow-other-keys)
  (let ((collisions (collisions (get-db-pathname) degree)))
    (dolist (collision collisions)
      (dolist (poly collision)
        (format *standard-output* "~a~%" poly))
      (terpri *standard-output*))))

(defmethod launch ((command (eql :process)) &key degree count workers &allow-other-keys)
  (declare (ignore count))
  (ensure-directories-exist *database-directory*)
  (handler-bind
      ((sb-sys:interactive-interrupt #'shutdown))
    (find-collisions (get-db-pathname) degree workers)))

(defun main ()
  (setq *random-state* (make-random-state t))
  (let ((arguments (cdr sb-ext:*posix-argv*)))
    (parse-commands arguments)))
