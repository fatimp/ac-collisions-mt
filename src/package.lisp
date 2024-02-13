(defpackage ac-collisions-mt
  (:use #:cl)
  (:local-nicknames (#:p    #:cl-polynomial/polynomial)
                    (#:u    #:cl-polynomial/util)
                    (#:zx   #:cl-polynomial/zx)
                    (#:si   #:stateless-iterators)
                    (#:alex #:alexandria)
                    (#:sera #:serapeum))
  (:export #:find-collisions
           #:random-polynomial
           #:collisions
           #:stop-workers
           #:summary))
