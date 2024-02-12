(in-package :ac-collisions-mt)

(sera:-> reverse-polynomial (p:polynomial)
         (values p:polynomial &optional))
(defun reverse-polynomial (f)
  (let ((d (p:degree f)))
    (p:polynomial
     (reduce
      (lambda (acc m)
        (u:bind-monomial (%d c) m
          (cons (cons (- d %d) c) acc)))
      (p:polynomial-coeffs f)
      :initial-value nil))))

(sera:-> random-polynomial (u:degree)
         (values p:polynomial &optional))
(defun random-polynomial (deg)
  (p:add
   p:+one+
   (p:multiply p:+variable+ (p:list->polynomial (loop repeat (1- deg) collect (random 2))))
   (p:polynomial (list (cons deg 1)))))

(sera:-> palindromep (p:polynomial)
         (values boolean &optional))
(defun palindromep (f)
  (p:polynomial= f (reverse-polynomial f)))

(sera:-> factorization-count-if (list (sera:-> (p:polynomial) (values boolean &optional)))
         (values (integer 0) &optional))
(defun factorization-count-if (list fn)
  (reduce #'+ list
          :key (lambda (factor)
                 (destructuring-bind (m . f) factor
                   (if (funcall fn f) m 0)))))


(sera:-> our-client-p (p:polynomial)
         (values boolean &optional))
(defun our-client-p (f)
  (> (factorization-count-if
      (zx:factor f) (alex:compose #'not #'palindromep))
     1))

(defun worker (deg)
  (pzmq:with-sockets ((worker-socket  :pub)
                      (control-socket :sub))
    (pzmq:setsockopt control-socket :subscribe nil)
    (pzmq:connect worker-socket  "inproc://data")
    (pzmq:connect control-socket "inproc://control")
    (let ((processed 0)
          (time (get-universal-time))
          found)
      (loop while t do
            (let ((poly (random-polynomial deg)))
              (when (our-client-p poly)
                (push (coerce (p:polynomial->list poly) 'bit-vector) found)))
            (incf processed)
            (when (> (get-universal-time) time)
              (pzmq:send worker-socket
                         (flexi-streams:with-output-to-sequence (out)
                           (cl-store:store (cons processed found) out)))
              (setq processed 0 found nil time (get-universal-time)))
            (ignore-errors
              (when (pzmq:recv-octets control-socket :dontwait t)
                (return nil)))))))

(defun start-worker (deg)
  (let ((context pzmq:*default-context*))
    (sb-thread:make-thread
     (lambda (deg)
       (let ((pzmq:*default-context* context))
         (worker deg)))
     :name "worker"
     :arguments (list deg))))
