;;;; package.lisp

(defpackage #:m-actors
  (:use #:cl #:bordeaux-threads #:priority-queue)
  (:export
    #:make-actor #:list-all-actors
    #:actor-send #:actor-receive
    #:actor-dead-p
    #:pop-error-log #:error-log-max-size
    #:actor-die #:*actor-self*)
  (:documentation
    "See README.md"))

(defpackage #:m-actors.test
  (:use #:cl #:m-actors #:bordeaux-threads)
  (:export
    #:ring-test))

