(in-package :ac-collisions-mt)

(defun prepare-database (db deg)
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists summary "
         "(degree integer primary key, processed integer not null, found integer not null);"))
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "create table if not exists collisions "
         "(poly blob not null unique, degree integer not null);"))
  (sqlite:execute-non-query
   db #.(concatenate
         'string
         "insert or ignore into summary (degree, processed, found) "
         "values (?, 0, 0);")
   deg))

(defun find-collisions (db-pathname deg nworkers)
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
              (loop while t do
                    (flexi-streams:with-input-from-sequence
                        (in (pzmq:recv-octets accum-socket))
                      (sqlite:with-transaction db
                        (destructuring-bind (processed . polys)
                            (cl-store:restore in)
                          (sqlite:execute-non-query
                           db #.(concatenate
                                 'string
                                 "update summary set "
                                 "processed = processed + ?, found = found + ? "
                                 "where degree = ?;")
                           processed (length polys) deg)
                          (dolist (poly polys)
                            (sqlite:execute-non-query
                             db
                             "insert or ignore into collisions (poly, degree) values (?, ?)"
                             poly deg))))))
            (stop-workers ()
              (pzmq:send control-socket nil)
              (mapc #'sb-thread:join-thread threads))))))))

(defun collisions (db-pathname deg)
  (sqlite:with-open-database (db (uiop:native-namestring (pathname db-pathname)))
    (mapcar (alex:compose #'p:sequence->polynomial #'car)
            (sqlite:execute-to-list db "select poly from collisions where degree = ?" deg))))

(defun summary (db-pathname)
  (sqlite:with-open-database (db (uiop:native-namestring (pathname db-pathname)))
    (sqlite:execute-to-list db "select * from summary")))
