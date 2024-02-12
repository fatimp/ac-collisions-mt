(in-package :ac-collisions-mt/cli)

(defparameter *database-directory* #p"~/.cache/ac-collisions/")

(defun print-usage-and-exit ()
  (format *error-output*
"Usage: ac-collisions-mt process degree [workers]
        ac-collisions-mt show [degree]~%")
  (uiop:quit 1))

(defun parse-integer-or-exit (string)
  (handler-case
      (parse-integer string)
    (sb-int:simple-parse-error () (print-usage-and-exit))))

(defun get-db-pathname ()
  (merge-pathnames "polynomials.db" *database-directory*))

(defun shutdown (c)
  (declare (ignore c))
  (format *standard-output* "Stopping worker threads~%")
  (invoke-restart 'stop-workers))

(defun process (options)
  (unless (<= 1 (length options) 2)
    (print-usage-and-exit))
  (let ((degree (parse-integer-or-exit (first options)))
        (workers (if (second options)
                     (parse-integer-or-exit (second options))
                     1)))
    (ensure-directories-exist *database-directory*)
    (handler-bind
        ((sb-sys:interactive-interrupt #'shutdown))
      (find-collisions (get-db-pathname) degree workers))))

(defun show-summary ()
  (let ((summary (summary (get-db-pathname))))
    (dolist (entry summary)
      (apply #'format *standard-output* "Degree=~d, Processed=~d, Collisions=~d~%"
             entry))))

(defun show-degree (deg)
  (let ((collisions (collisions (get-db-pathname) deg)))
    (dolist (collision collisions)
      (format *standard-output* "~a~%" collision))))

(defun show (options)
  (case (length options)
    (0 (show-summary))
    (1 (show-degree (parse-integer-or-exit (car options))))
    (t (print-usage-and-exit))))

(defun main ()
  (setq *random-state* (make-random-state t))
  (let ((arguments (cdr sb-ext:*posix-argv*)))
    (when (zerop (length arguments))
      (print-usage-and-exit))
    (destructuring-bind (command . options)
        arguments
      (cond
        ((string= command "process")
         (process options))
        ((string= command "show")
         (show options))
        (t
         (print-usage-and-exit))))))
