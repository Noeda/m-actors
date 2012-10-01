;;; messageholder.lisp
;;
;; This file implements a message holder.
;;
;; This is the actor that implements the behaviour of queueing messages
;; that are sent to named actors that don't exist (see ACTOR-SEND).
;;

(in-package #:m-actors)

(defmacro defactor (name initial-state
                         (msg-var state-var &optional timeout)
                         &body body)
  "This is a helper macro for defining a common type of actors.

Defines an actor function with the name NAME. INITIAL-STATE is a form that
is evaluated when a new actor is created with the resulting actor function.
MSG-VAR is a variable name that will hold the received message and
STATE-VAR is a variable that will hold the state of the actor. If TIMEOUT
is given, then, if no messages are received in the actor within the
TIMEOUT, a NIL message is given. BODY lists the forms that are evaluated
whenever a message arrives.

When BODY returns, the return value is assigned as new state and then the
actor returns to waiting a new message. As a convenience, if the symbol
M-ACTORS:DIE is sent to the actor, the actor invokes ACTOR-DIE on itself."
  (with-gensyms (state timeout-sym)
    `(defun ,name ()
       (let ((,state ,initial-state)
             (,timeout-sym ,timeout))
         (loop do
           (let* ((,state-var ,state)
                  (,msg-var (actor-receive t ,timeout-sym)))
             (when (equal ,msg-var 'die)
               (actor-die))
             ,@body))))))

(defstruct messageholder
  (names-to-queues (make-hash-table))
  (last-flush-time (now)))

(defactor messageholder (make-messageholder) (payload holder 1.0)
  (when payload (queue-up payload holder))
  (with-slots ((lft last-flush-time)) holder
    (when (< (1+ lft) (now))
      (setf lft (now))
      (flush-queues holder))))

(defun queue-up (payload holder)
  (with-slots ((queues names-to-queues)) holder
    (let ((name (car payload))
          (msg (cdr payload)))
      (mvb (queue present) (gethash name queues)
        (unless present
          (setq queue (make-queue :max-size 1000))
          (setf (gethash name queues) queue))
        (push-to-queue (cons msg (+ 60 (now))) queue)))))

(defun flush-queues (holder)
  (with-slots ((queues names-to-queues)) holder
    (let ((remove-names))

      (with-hash-table-iterator* (name queue queues)
        ;; We check if an actor exists by name NAME. It is possible that the
        ;; actor disappears check if an actor exists by name NAME. It is
        ;; possible the actor disappears after we checked that. In that case,
        ;; what happens is that the messages are sent back to this
        ;; messageholder actor. The damage is that the messages will live
        ;; somewhat longer. Not severe damage considering that it is rare for
        ;; the actor to disappear in this time scale.
        (loop do
          (mvb (msg present?) (peek-from-queue queue)
            (if present?
              (progn
                (if (actor-by-name name)
                  (let ((msg (pop-from-queue queue)))
                    (actor-send msg name))
                  (progn
                    (if (> (cdr msg) (now))
                      (pop-from-queue queue)
                      (return)))))
              (progn
                (push name remove-names)
                (return))))))

      ; Remove empty queues
      (dolist (n remove-names)
        (remhash n queues)))))



