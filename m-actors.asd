;;;; m-actors.asd

(asdf:defsystem #:m-actors
  :serial t
  :description "Simple actors for Common Lisp"
  :author "Mikko Juola <mikjuo@gmail.com>"
  :license "ISC license"
  :depends-on ("bordeaux-threads" "priority-queue" "m-util" "stefil")
  :components ((:file "package")
               (:file "queue")
               (:file "m-actors")
               (:file "convenience")
               (:file "messageholder")

               (:file "m-actors-ring-test")
               (:file "m-actors-faulty-actor-test")
               (:file "tests")))

