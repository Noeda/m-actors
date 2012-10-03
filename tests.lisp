;;;; tests.lisp
;;

(in-package #:m-actors.test)

(defsuite m-actors)
(in-suite m-actors)

(deftest initially-no-actors ()
  (sleep 1.0) ; clear residue from any other tests.
  (is (listp (list-all-actors)))
  (is (= (length (list-all-actors)) 0)))

(deftest make-actor-doesnt-crash ()
  (make-actor (lambda ())))

(deftest make-actor-with-name-doesnt-crash ()
  (make-actor (lambda ()) :name 'tatti))

(deftest make-actor-with-die-silently-doesnt-crash ()
  (make-actor (lambda ()) :die-silently nil))

(deftest make-actor-persists ()
  (let ((ac (make-actor (lambda () (sleep 2.0)))))
    (is (not (actor-dead-p ac)))
    (sleep 1.0)
    (is (not (actor-dead-p ac)))
    (sleep 2.0)
    (is (actor-dead-p ac))))

(define-condition test-1 (error) ())
(define-condition test-2 (error) ())
(define-condition test-3 (error) ())

(defmacro now-set (place)
  `(progn
     (is (not ,place))
     (setf ,place t)))

(deftest make-actor-silent-error ()
  ; flush all errors
  (loop while (pop-error-log) do (progn))

  (make-actor (lambda () (error 'test-1)))
  (make-actor (lambda () (error 'test-2)) :name 'tomaatti)
  (make-actor (lambda () (error 'test-3)))

  (sleep 0.5)
  (let ((testv-1) (testv-2) (testv-3))
    (loop do
      (let ((x (pop-error-log)))
        (cond
          ((and testv-1 testv-2 testv-3) (return))
          ((null x) (is nil) (return))
          ((equal (class-name (class-of x)) 'test-1) (now-set testv-1))
          ((equal (class-name (class-of x)) 'test-2) (now-set testv-2))
          ((equal (class-name (class-of x)) 'test-3) (now-set testv-3))))))
  t)

(deftest message-exchanging-works-by-actor ()
  (let* ((fail)
         (success)
         (ac1 (make-actor
                (lambda ()
                  (let ((f 0))
                    (loop do
                      (let ((val (actor-receive t)))
                        (when (eq val 'die) (return))

                        (unless (= val (1+ f))
                          (setq fail "VAL was not what I expected.")
                          (return))

                        (setq f val)
                        (if (= val 100)
                          (setq success t)
                          (setq success nil))))))))
         (ac2 (make-actor
                (lambda ()
                  (loop for x from 1 upto 100 do
                    (actor-send x ac1))))))
    (declare (ignorable ac2))
    (sleep 1.0)
    (actor-send 'die ac1)
    (is (not fail))
    (is success)))

(deftest actor-receive-fails-outside-actor ()
  (let ((*actor-self*))
    (is (handler-case (actor-receive)
          (not-in-actor () t)
          (error () nil)))))

(deftest message-exchanging-works-by-actor-name ()
  (let* ((fail)
         (success)
         (ac1 (make-actor
                (lambda ()
                  (let ((f 0))
                    (loop do
                      (let ((val (actor-receive t)))
                        (when (eq val 'die) (return))

                        (unless (= val (1+ f))
                          (setq fail "VAL was not what I expected.")
                          (return))

                        (setq f val)
                        (if (= val 100)
                          (setq success t)
                          (setq success nil))))))
                :name 'mursu))
         (ac2 (make-actor
                (lambda ()
                  (loop for x from 1 upto 100 do
                    (actor-send x 'mursu))))))
    (declare (ignorable ac2))
    (sleep 1.0)
    (actor-send 'die ac1)
    (is (not fail))
    (is success)))

(deftest named-actor-messages-are-queued ()
  (dotimes (i 100)
    (actor-send i 'henkselit))
  (sleep 0.5)

  (let ((incvalue 0))
    (make-actor
      (lambda ()
        (loop do
          (let ((val (actor-receive t 3.0)))
            (unless val (return))
            (incf incvalue val))))
      :name 'henkselit)

    (sleep 3.0)
    (is (= incvalue 4950))))

(deftest actor-die-really-kills-the-actor ()
  (loop while (pop-error-log) do (progn))

  (let ((ac (make-actor
              (lambda ()
                (loop do
                  (let ((val (actor-receive t)))
                    (when (eq val 'cookies)
                      (actor-die))))))))
    (is (member ac (list-all-actors)))
    (is (not (actor-dead-p ac)))
    (is (null (pop-error-log)))
    (sleep 0.5)
    (is (member ac (list-all-actors)))
    (is (not (actor-dead-p ac)))
    (is (null (pop-error-log)))
    (actor-send 'teepee ac)
    (sleep 0.5)
    (is (member ac (list-all-actors)))
    (is (not (actor-dead-p ac)))
    (is (null (pop-error-log)))
    (actor-send 'cookies ac)
    (sleep 0.5)
    (is (not (member ac (list-all-actors))))
    (is (actor-dead-p ac))))
    (is (null (pop-error-log)))

(deftest cannot-make-two-actors-with-same-name ()
  (make-actor (lambda () (sleep 2.0)) :name 'mushroom)
  (is (handler-case (make-actor (lambda () (sleep 2.5)) :name 'mushroom)
        (name-exists () t)
        (error () nil)))
  (sleep 3.0))

(deftest sending-messages-to-nonsense-doesnt-crash ()
  (actor-send (cons 'one 'two) 'three)
  (actor-send (cons 'one 'two) 'threee)
  (actor-send (cons 'one 'two) 17)
  (actor-send (cons 'one 'two) (make-actor (lambda ())))
  (actor-send 1929 1293139)
  (actor-send #(777 999 000) #(100 100))
  (actor-send "Hello" '(5 . 18))
  (actor-send (make-actor (lambda ())) (make-actor (lambda ()))))

(deftest actor-p-works ()
  (is (not (actor-p 15)))
  (is (not (actor-p (cons 'a 'b))))
  (is (not (actor-p "jungle")))
  (is (not (actor-p (lambda ()))))
  (is (not (actor-p #(1 2 3 4))))
  (is (not (actorp 15)))
  (is (not (actorp (cons 'a 'b))))
  (is (not (actorp "jungle")))
  (is (not (actorp (lambda ()))))
  (is (not (actorp #(1 2 3 4))))
  (is (actorp (make-actor (lambda ()))))
  (sleep 0.5))


