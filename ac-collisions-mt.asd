(defsystem :ac-collisions-mt
    :name :ac-collisions-mt
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Search for collisions in autocorrelation (multithreaded)"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "worker")
                 (:file "collisions"))
    :depends-on (:alexandria
                 :serapeum
                 :cl-polynomial
                 :pzmq
                 :sqlite
                 :flexi-streams
                 :cl-store))

(defsystem :ac-collisions-mt/cli
  :name :ac-collisions-mt/cli
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "cli"
  :serial t
  :components ((:file "package")
               (:file "cli"))
  :depends-on (:ac-collisions-mt
               :unix-opts)
  :build-operation program-op
  :build-pathname "ac-collisions"
  :entry-point "ac-collisions-mt/cli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
