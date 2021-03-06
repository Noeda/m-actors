;;; messageholder.lisp
;;
;; This file implements a message holder.
;;
;; This is the actor that implements the behaviour of queueing messages
;; that are sent to named actors that don't exist (see ACTOR-SEND).
;;

(in-package #:m-actors)

(defstruct messageholder
  (first-time t)
  (names-to-queues (make-hash-table))
  (last-flush-time (now)))

(defactor messageholder (make-messageholder) (payload holder 1.0)
  ; There's a tiny chance of a race condition. If the messageholder mail
  ; box is filled to maximum number of messages before we get here even
  ; once then the message holder crashes.
  (when (messageholder-first-time holder)
    (setf (messageholder-first-time holder) nil)
    (setf (actor-struct-dont-crash-on-mailbox-full *actor-self*) t))

  (when payload (queue-up payload holder))
  (with-slots ((lft last-flush-time)) holder
    (when (< (1+ lft) (now))
      (setf lft (now))
      (flush-queues holder)))
  holder)

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
                    (actor-send (car msg) name))
                  (progn
                    (if (< (cdr msg) (now))
                      (pop-from-queue queue)
                      (return)))))
              (progn
                (push name remove-names)
                (return))))))

      ; Remove empty queues
      (dolist (n remove-names)
        (remhash n queues)))))



