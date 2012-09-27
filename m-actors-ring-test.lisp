
(in-package #:m-actors.test)

(defvar *format-lock* (make-lock))

(defmacro locked-format (&rest args)
  `(with-lock-held (*format-lock*) (format ,@args)))

(defun ring-test ()
  "This is the traditional and silly ring test. 1000 actors are created and
  whenever one receives a message, it sends it to the next actor. The actors
  are linked in a ring. We can measure how long it takes for a message to 
  travel the entire ring or do some other performance tests.

  Tested implementations:

  SBCL (1.0.58) seems to easily handle 1000 threads. The results come rather
  quickly.

  Clozure CL (1.8) is much, much slower but will eventually produce results.

  CMU CL just gets stuck. I don't why.

  All the implementations eat up a lot of memory (>2 gigabytes) and virtual
  memory even more (might be problematic on 32-bit systems).
  
  The test is successful if 10 lines in the form 'final: N' are emitted where
  N is between 0 and 9."
  (let ((actors (loop for x from 1 to 1000 collect
                      (make-actor
                        (lambda ()
                          (let ((next-actor))
                            (loop do
                              (let ((msg (actor-receive t)))
                                (cond
                                  ((eql 'die msg) (actor-die))
                                  ((and (consp msg)
                                        (eql 'next-actor (car msg)))
                                   (setq next-actor (cdr msg)))
                                  (t
                                    (actor-send msg next-actor)))))))
                        :die-silently nil))))
    (let* ((last-actor (car actors))
           (first-actor last-actor))
      (mapc (lambda (actor)
              (actor-send (cons 'next-actor last-actor) actor)
              (setq last-actor actor))
            (cdr actors))
      (let ((hub-actor (make-actor
                         (lambda ()
                           (loop do
                             (let ((msg (actor-receive t)))
                               (when (eql msg 'die) (actor-die))
                               (locked-format t "final: ~a~%" msg)))))))
        (actor-send (cons 'next-actor hub-actor) first-actor)
        (dotimes (i 10)
          (actor-send i last-actor))
        (sleep 20.0)
        (actor-send 'die hub-actor)
        (mapc (lambda (actor)
                (actor-send 'die actor))
              actors)
        t))))


