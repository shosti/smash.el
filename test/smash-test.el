;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'smash)

(ert-deftest smash-nil ()
  (should (equal (%car nil) nil))
  (should (equal (%cdr nil) nil)))

(ert-deftest smash-shorthand ()
  (should (equal (%stream) nil))
  (should (equal (%stream->list (%stream 3 8 1 :foo))
                 '(3 8 1 :foo))))


(ert-deftest smash-take ()
  (should (equal (%stream->list (%take 3 (%stream 8 1 3 2 8)))
                 '(8 1 3)))
  (should (equal (%stream->list (%take 5 (%stream 1 2 3)))
                 '(1 2 3)))
  (should (equal (%take 50 nil) nil)))

(ert-deftest smash-infinite-primes ()
  (defun divisible? (x y)
    (= (% x y) 0))

  (defun prime-sieve (list)
    (%cons
     (%car list)
     (prime-sieve (%filter
             (lambda (x)
               (not (divisible? x (%car list))))
             (%cdr list)))))

  (let ((primes (prime-sieve (%iterate '1+ 2))))
    (should (equal (%stream->list (%take 8 primes))
                   '(2 3 5 7 11 13 17 19)))))

(provide 'smash-test)

;;; test-smash.el ends here
