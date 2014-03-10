;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'smash)

(ert-deftest smash-nil ()
  (should (equal (%car nil) nil))
  (should (equal (%cdr nil) nil)))

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
