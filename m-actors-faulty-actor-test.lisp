;;; m-actors-faulty-actor-test.lisp
;; See docstring on faulty-actor-test function.
;;

(in-package #:m-actors.test)

(defun faulty-actor-test ()
  "This test tests if naming system works. It creates a 'faulty' actor that
  crashes often and a supervisor that restarts the actor whenever it
  detects the actor is not up and running. Another actor asks it to double
  numbers and we count the number of times it can come up with an answer.
  The test is successful if, despite crashings, it can come up with an
  answer most of the time.
  
  This test is not fully deterministic. There is a chance it fails even if
  the implementation is correct; it could fail in a very slow environment.

  It is unfortunate but if the test itself crashes/throws error/something evil
  then stray actors may be left running.
  
  The test can take a few minutes to complete.
  
  Returns T if the test was successful and NIL if it wasn't."
  (make-actor #'faulty-supervisor :name 'faulty-supervisor :die-silently nil)
  (let ((answers 0)
        (complete nil)
        (lock (make-lock))
        (sema (make-condition-variable)))
    (make-actor (lambda ()
                  (dotimes (b 100)
                    (actor-send (cons *actor-self* b) 'faulty-doubler)
                    (let ((val (actor-receive t 3.0)))
                                  (if (and val (= (* 2 (car val))
                                                  (cdr val)))
                                    (progn
                                      (incf answers)
                                      nil))))
                  (with-lock-held (lock)
                    (setq complete t)
                    (condition-notify sema)))
                :die-silently nil)
    (with-lock-held (lock)
      (loop until complete do
        (condition-wait sema lock)))
    (actor-send 'die 'faulty-supervisor)
    (sleep 1)
    (actor-send 'die 'faulty-doubler)
    (if (>= answers 50)
      t
      nil)))

;; This actor supervises the faulty actor; it restarts it whenever it
;; detects it is gone.
(defactor faulty-supervisor nil (msg dummy 1.0)
  (when (actor-by-name 'faulty-doubler) (return-from faulty-supervisor))

  ; Pop error accumulated messages.
  (pop-error-log)
  (pop-error-log)

  (make-actor #'faulty-doubler :name 'faulty-doubler)
  nil)

;; This actor doubles numbers. However, it is 'faulty' and tends to crash
;; while doing it.
(defactor faulty-doubler nil (msg dummy)
  (unless msg (return-from faulty-doubler))

  ; Crash 10% of time
  (let ((*random-state* (make-random-state t)))
    (if (= (random 10) 5) (error "I just crashed.")))

  (let ((return-actor (car msg))
        (val (cdr msg)))
    (actor-send (cons val (* 2 val)) return-actor))
  t)

