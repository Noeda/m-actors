;;;; m-actors.lisp

(in-package #:m-actors)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (import 'm-util:now) 'm-actors)

(defconstant +actor-mailbox-queue-size+ 20000)

(defstruct actor-struct
  (thread nil)
  (name nil)
  (dont-crash-on-mailbox-full nil)
  (mail-box-cond (make-condition-variable))
  (mail-box (make-locked-object
              (make-queue :max-size +actor-mailbox-queue-size+))))

;; In the some cases you need the lock that is usually private
;; within the closure created by MAKE-LOCKED-OBJECT. It is bound to this
;; special variable whenever a lock is made.
(defvar *locked-object-lock* nil)

(defun make-locked-object (object)
  "Creates a closure that, when called with a function, calls that function
  with the object while a lock is held.

Makes it harder to accidentally use a shared object between threads,
causing undefined behaviour."
  (let ((lock (make-lock)))
    (lambda (func)
      (with-lock-held (lock)
                      (let ((*locked-object-lock* lock))
                        (funcall func object))))))

;; A macro to make code more concise using the above function
(defmacro with-locked-object ((var locked-object) &body body)
  `(funcall ,locked-object (lambda (,var) ,@body)))

(defvar *actor-self* nil
  "This special variable refers to the actor the current code is
   running in. It is NIL for threads that were not created with
   MAKE-ACTOR.")

(defvar *cond-notifier-thread* nil
  "This will hold the thread object that notifies condition variables
  when ACTOR-RECEIVE is called with blocking and timeout. This thread
  is started on demand. Use *COND-NOTIFIER-LOCK* before access.")
(defvar *cond-notifier-lock* (make-lock)
  "Lock for *COND-NOTIFIER-THREAD*. Lock before you use it.")
(defvar *cond-notifier-pqueue* (make-locked-object (make-pqueue
                                                     #'<
                                                     :key-type 'float
                                                     :value-type 'cons)))

(defun condition-notify-after-timeout (lock cond-var timeout)
  (declare (type real timeout))
  (let ((timeout (max 0.0 (coerce timeout 'float))))
    (with-lock-held (*cond-notifier-lock*)
      (unless *cond-notifier-thread*
        (setq *cond-notifier-thread*
              (make-thread
                (lambda ()
                  (loop
                    (sleep 1.0)
                    (let ((now (now))
                          (notify-us))
                      (with-locked-object (pqueue
                                            *cond-notifier-pqueue*)
                        (loop while (and (not (pqueue-empty-p pqueue))
                                         (< (pqueue-front-key pqueue) now))
                              do
                          (let ((item (pqueue-pop pqueue)))
                            (push item notify-us))))
                      (dolist (n notify-us)
                        (with-lock-held ((car n))
                          (condition-notify (cdr n)))))))))))
    (with-locked-object (pqueue *cond-notifier-pqueue*)
      (pqueue-push (cons lock cond-var) (+ (now) timeout) pqueue))
    t))

;; This is the global variable that holds all the error messages.
;; It's a queue so can't be defined as a simple list.
(defvar *error-queue* (make-locked-object (make-queue :max-size 1000)))

;; This one holds all the actors in one big merry hash table.
(defvar *actors* (make-locked-object (make-hash-table)))

;; And this one holds names.
(defvar *actor-names* (make-locked-object (make-hash-table)))

;; Message holder actor.
(defvar *message-holder-actor* nil)
(defvar *message-holder-actor-lock* (make-lock))

;; NOTE: this function is not public. I can't come up with good reasons why
;; it should be.
(defun push-actor-error (error-condition)
  "Pushes a new error to the error log queue. If the error queue is full
  (see ERROR-LOG-MAX-SIZE) then does nothing. The error log is global but
  this function can be safely called from any thread at any time.
  
  Returns T if the error was enqueued and NIL if it wasn't (because of
  size limits).
  
  You can't push NIL as an error because it is used to indicate no errors
  in POP-ERROR-LOG. This function raises an error if you try to use NIL."
  (when (null error-condition)
    (error "Error to be enqueued to the error log queue must not be NIL."))

  (with-locked-object (queue *error-queue*)
    (push-to-queue error-condition queue)))

(defun pop-error-log ()
  "Pops an error message from error log queue. If there are no new
  errors, returns NIL.
  
  This function can be safely called from any thread. Do know that the
  error log is a globally shared data structure so the thread that gets
  an error is not going to be returned in any other thread."
  (with-locked-object (queue *error-queue*)
    (values (pop-from-queue queue)))) ; suppress needless second value

(defun error-log-max-size ()
  "When called, returns the current maximum size of the error queue.
  By default, the limit is 1000. You can change log size with setf,
  e.g. (SETF (ERROR-LOG-MAX-SIZE) 2000).
  
  Changing the limit does not truncate the current errors in the queue
  but it prevents new ones from coming until the size of the queue becomes
  less than the maximum size.
  
  Changing the limit can be done safely from any thread at any time."
  (with-locked-object (queue *error-queue*)
    (queue-max-size queue)))

(defun set-error-log-max-size (new-size)
  (with-locked-object (queue *error-queue*)
    (setf (queue-max-size queue) new-size)))

(defsetf error-log-max-size set-error-log-max-size)

;; The purpose of this condition is to avoid going to debugger
;; when ACTOR-DIE is called (it raises a signal of this condition)
(define-condition intentionally-died (error)
  ((actor :initarg :actor :reader actor)
   (has-error-object :initform nil
                     :initarg :has-error-object
                     :reader has-error-object)
   (error-object :initform nil
                 :initarg :error-object
                 :reader error-object)))

(define-condition not-in-actor (error) ())

(define-condition too-many-messages (error)
  ((actor :initarg :actor :reader actor)))

(define-condition name-exists (error)
  ((name :initarg :name :reader name)
   (offender :initarg :offender :reader offender)))

(defun make-actor (func &key (die-silently t) (name nil))
  "Makes a new actor. It runs function FUNC inside the actor.
  If DIE-SILENTLY is true (the default), then any error inside func
  that is not caught is pushed into the error log instead of (possibly)
  going into the debugger. See function POP-ERROR-LOG for the error log stuff.

  If NAME is not nil, then it the actor will have that name. Only one actor
  with a specific name can exist. An error NAME-EXISTS will be raised if
  you attempt to create an actor with the same name as an existing actor.
  NAME can technically be any kind of value; it will be compared with EQL.
  Be aware that it is captured so if it's the sort of value you might
  modify, then the name may get modified inside the actor system as well.

  The purpose of dying \"silently\" is meant to provide fault tolerance.
  Dying actors send dying messages to linked actors that can then take
  some action, if desired.

  Returns the actor. The actor may not necessarily be of any specific type."
  (declare (type function func))

  ;; If name is given, check that it is free and claim it as well.
  (if name
    (progn
      (with-locked-object (names *actor-names*)
        (awhen (gethash name names) (error 'name-exists
                                           :name name
                                           :offender it))
        (setf (gethash name names) t))
      (unwind-protect-if-fails
          (make-actor-with-name func die-silently name)
        (with-locked-object (names *actor-names*)
          (remhash name names))))
    (make-actor-with-name func die-silently nil)))

(defun make-actor-with-name (func die-silently name)
  (declare (type function func))

  (let ((new-actor (make-actor-struct))
        ;; the purpose of these three variables is to not
        ;; let the thread start before we can reliably set a completed thread
        ;; struct inside *actor-self* in the thread code.   
        (cond-var (make-condition-variable))        ;      |
        (cond-var-lock (make-lock))                 ;      |
        (cond-var-go-ahead nil))                    ;      |
    (let ((thr                                      ;      |
            (make-thread                            ;      |
              (lambda ()                            ;      |
                                                    ;      | 
                ;; wait until the aformentioned condition  } here
                (with-lock-held (cond-var-lock)
                  (loop until cond-var-go-ahead do
                    (condition-wait cond-var cond-var-lock)))

                (unwind-protect
                    (flet ((call-func ()
                             (handler-case (funcall func)
                               (intentionally-died (condition)
                                 (when (has-error-object condition)
                                   (push-actor-error (error-object condition)))
                                 t))))
                      (let ((*actor-self* new-actor))
                        (cond
                          (die-silently
                            (multiple-value-bind (success? error-condition)
                                (ignore-errors (values t (call-func)))
                              (unless success?
                                (push-actor-error error-condition))))
                          (t (call-func)))))
                  (progn
                    (when name
                      (with-locked-object (names *actor-names*)
                        (remhash name names)))
                    (with-locked-object (actors *actors*)
                      (remhash new-actor actors))))))))
      (setf (actor-struct-thread new-actor) thr)
      (with-locked-object (actors *actors*)
        (setf (gethash new-actor actors) t))
      (when name
        (with-locked-object (names *actor-names*)
          (assert (eq t (gethash name names)))
          (setf (actor-struct-name new-actor) name)
          (setf (gethash name names) new-actor)))
      (with-lock-held (cond-var-lock)
        (setq cond-var-go-ahead t)
        (condition-notify cond-var))
      new-actor)))

(defun actor-dead-p (actor)
  "Returns T if the actor pointed by ACTOR is dead."
  (with-locked-object (actors *actors*)
    (if (gethash actor actors) nil t)))

(defun actor-name (actor)
  "Returns the name of the actor ACTOR or NIL if the actor doesn't have a
  name."
  (actor-struct-name actor))

(defun actor-by-name (name)
  "Returns the actor pointed by NAME, or NIL if there's no actor with that
  name."
  (with-locked-object (names *actor-names*)
    (values (gethash name names))))

(defun actor-die (&optional (error-object nil error-object-supplied))
  "Makes the currently running actor die. It is an error to call this
   function in a thread that was not created with MAKE-ACTOR. Making the
   actor die with this function does not raise an error.

   If the optional parameter ERROR-OBJECT is given, then it will be put
   in the error log (assuming it is not full). ERROR-OBJECT can't be NIL
   (if it is, this will behave as if you didn't supply an error object)."

  ; give a more meaningful error message if misused
  (unless *actor-self*
    (error "ACTOR-DIE called in a thread that was not created with ~
            MAKE-ACTOR."))

  (if (and error-object-supplied error-object)
    (error 'intentionally-died :actor *actor-self*
                               :has-error-object t
                               :error-object error-object)
    (error 'intentionally-died :actor *actor-self*)))

(defun list-all-actors ()
  "Returns a fresh list of all currently running actors made with MAKE-ACTOR."
  (let ((result))
    (with-locked-object (actors *actors*)
      (with-hash-table-iterator (generator actors)
        (loop
          (multiple-value-bind (more? thread value) (generator)
            (declare (ignore value))
            (unless more? (return))
            (push thread result)))))
    result))

(defun start-message-holder-actor ()
  (with-lock-held (*message-holder-actor-lock*)
    (when *message-holder-actor* (return-from start-message-holder-actor))

    (setq *message-holder-actor* (make-actor #'messageholder
                                             :die-silently nil))))

(defun actor-send (object actor-or-name)
  "Sends a message to an actor. The actor code can use ACTOR-RECEIVE to
  receive it.

  There is a limit on how many messages can be sent to an actor. If an actor
  has too many messages in it, it will die when the message box is used with
  ACTOR-RECEIVE. Also, when the limit is reached, any attempt to send
  anything to the actor is a no-op, effective immediately when
  the limit is reached (even before the error is raised in the receiving
  actor).

  If ACTOR is not an actor object, then it is assumed to be a name that
  points to an actor. If there's no actor by that name, then the message is
  queued for 60 seconds to wait until an actor of that name is created.
  When such actor is created, it is not guaranteed that all the messages
  arrive or that they arrive immediately. This behaviour merely makes such
  occurrence less likely to happen. Most notably, if an actor crashes, all
  the messages currently in its queue are lost in any case.

  If there's no actor by that name, this function does nothing.

  A message can be any Lisp object. It's probably for the best that you do
  not attempt to use the object after sending it away to avoid different
  threads of execution of using the same object at the same time. A notable
  exception is the actor objects themselves; it is safe for many threads of
  execution to send messages through the same actor object.

  This function returns immediately after the message was put in the
  receiving actor's \"mailbox\". This means this function, in most cases,
  returns before.

  the message is processed in the target actor. There is no guarantee that
  the message is going to be processed; that depends on the code of the
  receiving actor.

  Always returns T. Even if the message wasn't sent or can't be received."
  (if (actor-struct-p actor-or-name)
    (with-locked-object (mail-box (actor-struct-mail-box actor-or-name))
      (push-to-queue object mail-box)
      (condition-notify (actor-struct-mail-box-cond actor-or-name)))
    (let ((ac (actor-by-name actor-or-name)))
      (if ac
        (actor-send object ac)
        (progn
          (start-message-holder-actor)
          (actor-send (cons actor-or-name object)
                      *message-holder-actor*)))))
  t)

(defun actor-receive (&optional block timeout)
  "Returns an object that was sent to the actor with the use of ACTOR-SEND.

  To be more precise, returns two values. The first one is the object. The
  second one is T or NIL, depending on whether there was an object in the
  mailbox that was returned. T means there was one. NIL means there wasn't.
  Therefore, when the mailbox was empty, returns (VALUES NIL NIL).

  If the optional parameter BLOCK is true, then this function will block until
  a message arrives.

  If BLOCK is given and true, TIMEOUT may also be supplied. If no message is
  received within TIMEOUT seconds, then returns (VALUES NIL NIL). TIMEOUT
  has bad resolution (at worst it can miss with about 1 second) due to
  implementation difficulties. Don't count on it being accurate with timing.
  TIMEOUT is given as an integer or a float and it depicts seconds (like
  Common Lisp SLEEP function).

  It is an error to call this function outside actor code. An error will be
  raised if you try.

  If you call this outside any actor code, NOT-IN-ACTOR condition is raised.

  It is possible for the actor to die during ACTOR-RECEIVE if the mailbox of
  the actor hit the maximum message limit. Depending on SILENTLY-DIE
  keyword parameter in MAKE-ACTOR and CL implementation, this may or may not
  fire a debugger."

  (unless *actor-self*
    (error 'not-in-actor))

  (unless (or (null timeout)
              (realp timeout))
    (error "TIMEOUT must be either NIL or a real number."))

  (if block
    (actor-receive-blocking timeout)
    (with-locked-object (mail-box (actor-struct-mail-box *actor-self*))
      (when (and (queue-max-size-trigger mail-box)
                 (not (actor-struct-dont-crash-on-mailbox-full *actor-self*)))
        (error 'too-many-messages :actor *actor-self*))
      (pop-from-queue mail-box))))

(defun actor-receive-blocking (timeout)
  (with-locked-object (mail-box (actor-struct-mail-box *actor-self*))
    (with-slots ((cond-var mail-box-cond)) *actor-self*
      (let ((time-in-future (if timeout
                              (+ (now) timeout)
                              nil)))

        (when (and timeout (> timeout 0.0))
          (condition-notify-after-timeout *locked-object-lock*
                                          cond-var timeout))

        (loop until (or (> (queue-size mail-box) 0)
                        (and timeout (>= (now) time-in-future))) do
          (condition-wait cond-var
                          *locked-object-lock*))

        (if (> (queue-size mail-box) 0)
          (pop-from-queue mail-box)
          (values nil nil))))))

