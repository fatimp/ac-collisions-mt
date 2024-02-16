(in-package :ac-collisions-mt)

(sera:defconstructor report
  (processed   integer)
  (irreducible integer)
  (found       list))

(sera:-> reverse-polynomial (p:polynomial)
         (values p:polynomial &optional))
(defun reverse-polynomial (f)
  (let ((d (p:degree f)))
    (p:polynomial
     (reduce
      (lambda (acc m)
        (p:bind-monomial (%d c) m
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

(sera:-> collidesp (list)
         (values boolean &optional))
(defun collidesp (factors)
  (> (factorization-count-if factors (alex:compose #'not #'palindromep)) 1))

(sera:-> irreduciblep (list)
         (values boolean &optional))
(defun irreduciblep (factors)
  (= (factorization-count-if factors (constantly t)) 1))

(defun worker (deg)
  (pzmq:with-sockets ((worker-socket  :pub)
                      (control-socket :sub))
    (pzmq:setsockopt control-socket :subscribe nil)
    (pzmq:connect worker-socket  "inproc://data")
    (pzmq:connect control-socket "inproc://control")
    (let ((processed 0)
          (irreducible 0)
          (time (get-universal-time))
          found)
      (loop for poly = (random-polynomial deg)
            for factors = (zx:factor poly)
            while t do
            (when (collidesp factors)
              (push (coerce (p:polynomial->list poly) 'bit-vector) found))
            (when (irreduciblep factors)
              (incf irreducible))
            (incf processed)
            (when (> (get-universal-time) time)
              (pzmq:send worker-socket
                         (flexi-streams:with-output-to-sequence (out)
                           (cl-store:store (report processed irreducible found) out)))
              (setq processed   0
                    irreducible 0
                    found       nil
                    time        (get-universal-time)))
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
