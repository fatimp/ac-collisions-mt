(in-package :ac-collisions-mt)

(defun prepare-database (db deg)
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists summary "
         "(degree integer primary key, processed integer not null, "
         "found integer not null, irreducible integer not null);"))
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists collisions "
         "(poly blob not null unique, degree integer not null);"))
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "insert or ignore into summary (degree, processed, found, irreducible) "
         "values (?, 0, 0, 0);")
   deg))

(defun stop-workers (socket threads)
  (pzmq:send socket nil)
  (mapc #'sb-thread:join-thread threads))

(defun find-collisions (db-pathname deg nworkers &optional count)
  (pzmq:with-context (ctx :io-threads 0)
    (pzmq:with-sockets ((control-socket :pub)
                        (accum-socket   :sub))
      (pzmq:setsockopt accum-socket :subscribe nil)
      (pzmq:bind control-socket "inproc://control")
      (pzmq:bind accum-socket   "inproc://data")

      ;; Prepare database
      (sqlite:with-open-database (db (uiop:native-namestring (pathname db-pathname)))
        (prepare-database db deg)

        ;; Start workers
        (let ((threads (loop repeat nworkers collect (start-worker deg))))
          (restart-case
              (loop with total = 0
                    while (or (not count) (< total count))
                    for report = (flexi-streams:with-input-from-sequence
                                     (in (pzmq:recv-octets accum-socket))
                                   (cl-store:restore in))
                    for processed   = (report-processed report)
                    for irreducible = (report-irreducible report)
                    for found       = (report-found report)
                    do
                    (sqlite:with-transaction db
                      (sqlite:execute-non-query
                       db #.(concatenate
                             'string
                             "update summary set "
                             "processed = processed + ?, found = found + ?, "
                             "irreducible = irreducible + ? "
                             "where degree = ?;")
                       processed (length found) irreducible deg)
                      (dolist (poly found)
                        (sqlite:execute-non-query
                         db
                         "insert or ignore into collisions (poly, degree) values (?, ?)"
                         poly deg)))
                    (incf total processed)
                    finally (return (stop-workers control-socket threads)))
            (stop-workers ()
              (stop-workers control-socket threads))))))))

(defun unroll-factors (factors)
  (labels ((%unroll (factor acc)
             (destructuring-bind (m . f) factor
               (append (loop repeat m collect f) acc))))
    (reduce #'%unroll factors
            :initial-value nil
            :from-end t)))

(defun construct-collisions (f)
  (multiple-value-bind (palindromes non-palindromes)
      (sera:partition #'palindromep (unroll-factors (zx:factor f)))
    (let ((sym-part (reduce #'p:multiply palindromes :initial-value (car non-palindromes)))
          (non-sym-part (cdr non-palindromes)))
      (si:collect
          (si:imap
           (lambda (fns)
             (apply #'p:multiply sym-part
                    (mapcar #'funcall (alex:flatten fns) non-sym-part)))
           (reduce #'si:product
                   (loop repeat (length non-sym-part) collect
                         (si:list->iterator (list #'identity #'reverse-polynomial)))))))))

(defun collisions (db-pathname deg)
  (sqlite:with-open-database (db (uiop:native-namestring (pathname db-pathname)))
    (mapcar (alex:compose #'construct-collisions #'p:sequence->polynomial #'car)
            (sqlite:execute-to-list db "select poly from collisions where degree = ?" deg))))

(defun summary (db-pathname)
  (sqlite:with-open-database (db (uiop:native-namestring (pathname db-pathname)))
    (sqlite:execute-to-list db "select * from summary")))
