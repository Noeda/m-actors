;;;; convenience.lisp

(in-package #:m-actors)

(defmacro actor-lambda (initial-state
                           (msg-var state-var &optional timeout dont-catch-die)
                           &body body)
  "This is a helper macro for defining a common type of actors.

Creates an anonymous actor function. INITIAL-STATE is a form that is
evaluated when a new actor is created with the resulting actor function.
MSG-VAR is a variable name that will hold the received message and
STATE-VAR is a variable that will hold the state of the actor. If TIMEOUT
is given, then, if no messages are received in the actor within the
TIMEOUT, a NIL message is given. BODY lists the forms that are evaluated
whenever a message arrives.

When BODY returns, the return value is assigned as new state and then the
actor returns to waiting a new message. As a convenience, if the symbol
M-ACTORS:DIE is sent to the actor, the actor invokes ACTOR-DIE on itself,
unless DONT-CATCH-DIE is true. You probably should handle DIE then yourself."
  (with-gensyms (state timeout-sym)
    `(lambda ()
       (let ((,state ,initial-state)
             (,timeout-sym ,timeout))
         (loop do
           (let* ((,state-var ,state)
                  (,msg-var (actor-receive t ,timeout-sym)))
             (declare (ignorable ,state-var ,msg-var))
             ,(unless dont-catch-die
                `(when (equal ,msg-var 'die)
                   (actor-die)))
             (setq ,state
                   (progn ,@body))))))))

(defmacro defactor (name initial-state
                         (msg-var state-var &optional timeout dont-catch-die)
                         &body body)
  "This is the same as ACTOR-LAMBDA but binds the actor function to a
name, You could say DEFACTOR is to ACTOR-LAMBDA as DEFUN is to LAMBDA."
  `(defun ,name ()
     (funcall (actor-lambda ,initial-state
                            (,msg-var ,state-var ,timeout ,dont-catch-die)
                            ,@body))))

(defmacro 1-1-supervisor-lambda (&rest funs-and-names)
  "Creates a simple 1 for 1 supervisor function. This supervisor, when
  launched, will check that an actor with given names exist. If one of them
  don't, then it is launched. The checks are made at every 2 seconds.

  Te parameters are lists of two elements, the first one is the
  name of the actor and second one is the function to launch in an actor of that
  name.

  For example, (1-1-supervisor-lambda ('database #'database-fun)
                                      ('frontend #'frontend-fun)
                                      ('backend #'backend-fun))

  Each name and function is evaluated at the time the lambda is created.

  See DEFINE-1-1-SUPERVISOR.
  "
  (with-gensyms (msg state names funs name fun)
    `(let ((,names (list ,@(mapcar (lambda (i) (car i)) funs-and-names)))
           (,funs (list ,@(mapcar (lambda (i) (second i)) funs-and-names))))
       (actor-lambda nil (,msg ,state 2.0 t)
          (when (eql ,msg 'm-actors:die)
            (dolist (,name ,names)
              (actor-send 'm-actors:die ,name))
            (actor-die))
          (mapc (lambda (,name ,fun)
                  (unless (actor-by-name ,name)
                    (make-actor ,fun :name ,name)))
                ,names ,funs)
          nil))))

(defmacro define-1-1-supervisor (name &rest funs-and-names)
  "This function defines a global supervisor function.
  DEFINE-1-1-SUPERVISOR is to 1-1-SUPERVISOR-LAMBDA as DEFUN is to LAMBDA."
  `(setf (symbol-function ,name) (1-1-supervisor-lambda ,@funs-and-names)))

