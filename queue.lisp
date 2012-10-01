;;;; queue.lisp
;;;
;;; This file has a queue implementation.
;;;

(in-package #:m-actors)

(defstruct queue
  (front nil :type (or null cons))
  (end nil :type (or null cons))
  (size 0 :type fixnum)
  (max-size nil :type (or null fixnum))
  (max-size-trigger nil :type (or null t)))

(defun push-to-queue (object queue)
  "Pushes OBJECT to QUEUE. If queue has a maximum size, may not actually push
  the object. By default queues don't have a maximum size.
  Returns T if the object was pushed and NIL if it wasn't.
  
  QUEUE-MAX-SIZE-TRIGGER function on the queue will return T if there was an
  attempt to push to the queue while it was full. The flag is cleared when any
  item is popped from the queue (excluding trying to pop from an empty queue)."
  (with-slots ((front front) (end end)
               (size size) (max-size max-size)
               (max-size-trigger max-size-trigger)) queue
    (cond
      ((or (null max-size) (< size max-size))
       (let ((list-end (cons object nil)))
         (when end
           (setf (cdr end) list-end))
         (setf end list-end)
         (when (= size 0) (setf front end))
         (incf size)
         t))
      (t (setf max-size-trigger t)
         nil))))

(defun peek-from-queue (queue)
  "Same as POP-FROM-QUEUE but doesn't remove the object from the queue nor
  does this reset any max size triggers."
  (with-slots ((front front)
               (size size)) queue
    (if (> size 0)
      (values (car front) t)
      (values nil nil))))

(defun pop-from-queue (queue)
  "Takes one object from the queue and returns it. Returns two values:
  the object and T if there was an object in the queue and NIL if there
  wasn't (to distinguish from NIL values in the queue). That is, if the queue
  is empty, two values, both NIL, are returned."
  (with-slots ((front front) (end end)
               (size size) (max-size-trigger max-size-trigger)) queue
    (cond
      ((> size 0)
       (setf max-size-trigger nil)
       (multiple-value-prog1
         (values (car front) t)
         (if (= size 1)
           (setf front nil
                 end nil
                 size 0)
           (setf front (cdr front)
                 size (1- size)))))
      (t
        (values nil nil)))))

