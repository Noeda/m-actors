;;;; package.lisp

(defpackage #:m-actors
  (:use #:cl #:m-util #:bordeaux-threads #:priority-queue)
  (:export
    #:make-actor #:list-all-actors
    #:actor-send #:actor-receive
    #:actor-dead-p
    #:actor-name
    #:actor-by-name

    #:die

    #:too-many-messages
    #:name-exists
    #:actor
    #:name
    #:offender

    #:actor-lambda
    #:defactor
    #:1-1-supervisor-lambda
    #:define-1-1-supervisor

    #:pop-error-log #:error-log-max-size
    #:actor-die #:*actor-self*)
  (:documentation
    "See README.md"))

(defpackage #:m-actors.test
  (:use #:cl #:m-actors #:bordeaux-threads)
  (:export
    #:ring-test
    #:faulty-actor-test))

