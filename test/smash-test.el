;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'smash)

(defun even? (x) (= (% x 2) 0))

(ert-deftest smash-nil ()
  (should (equal (%car nil) nil))
  (should (equal (%cdr nil) nil)))

(ert-deftest smash-cons ()
  (should (equal (%car (%cons 5 nil)) 5))
  (should (equal (%cdr (%cons 5 nil)) nil))
  (should (equal (%cdr (%cons 5 3)) 3)))

(ert-deftest smash-lists ()
  (should (equal (%cdr (%list->stream '(3))) nil))
  (let ((items '(2 8 1 7 "blue" fish :red)))
    (should (equal (%car (%list->stream items))
                   2))
    (should (equal (%stream->list (%list->stream items))
                   items))))

(ert-deftest smash-shorthand ()
  (should (equal (%stream) nil))
  (should (equal (%stream->list (%stream 3 8 1 :foo))
                 '(3 8 1 :foo))))

(ert-deftest smash-stream? ()
  (should (%stream? (%cons 3 nil)))
  (should (%stream? (%stream 1 2 3)))
  (should-not (%stream? (list 1 2 3)))
  (should-not (%stream? 3))
  (should-not (%stream? nil)))

(ert-deftest smash-nth ()
  (should (equal (%nth 1 nil) nil))
  (let ((stream (%stream 2 8 1 7 "blue" :red)))
    (should (equal (%nth 4 stream) "blue"))
    (should (equal (%nth 8 stream) nil))))

(ert-deftest smash-take ()
  (should (equal (%stream->list (%take 3 (%stream 8 1 3 2 8)))
                 '(8 1 3)))
  (should (equal (%stream->list (%take 5 (%stream 1 2 3)))
                 '(1 2 3)))
  (should (equal (%take 50 nil) nil)))

(ert-deftest smash-take-while ()
  (should (equal (%stream->list (%take-while (lambda (x) (< x 7))
                                             (%stream 2 3 1 7 2)))
                 '(2 3 1))))

(ert-deftest smash-drop ()
  (should (equal (%stream->list (%drop 3 (%stream 1 2 3 4 5)))
                 '(4 5)))
  (should (equal (%drop 3 (%stream 1 2)) nil)))

(ert-deftest smash-drop-while ()
  (should (equal (%stream->list (%drop-while 'even? (%stream 2 8 0 1 2)))
                 '(1 2)))
  (should (equal (%drop-while 'even? (%stream 2 4 6 8)) nil))
  (should (equal (%drop-while 'even? nil) nil)))

(ert-deftest smash-map ()
  (should (equal (%stream->list (%map '1+ (%stream 8 1 2 7)))
                 '(9 2 3 8)))
  (should (equal (%map '1+ nil) nil)))

(ert-deftest smash-reduce ()
  (should (equal (%reduce '+ (%stream 1 2 3 4)) 10))
  (should (equal (%reduce '+ nil) nil)))

(ert-deftest smash-reduce-from ()
  (should (equal (%reduce-from '+ 5 (%stream 1 2 3 4)) 15))
  (should (equal (%reduce-from '+ 3 nil) 3)))

(ert-deftest smash-filter ()

  (should (equal (%stream->list (%filter 'even? (%stream 1 2 3 4 5 6 7)))
                 '(2 4 6)))
  (should (equal (%filter 'even? nil) nil)))

(ert-deftest smash-contains? ()
  (should (%contains? (%stream 1 8 2 5 6) 5))
  (should-not (%contains? (%stream 1 8 2 5 6) 4))
  (should-not (%contains? nil 7)))

(ert-deftest smash-any? ()
  (should (%any? 'even? (%stream 3 1 7 8)))
  (should-not (%any? 'even? (%stream 71 83 1 7)))
  (should-not (%any? 'even? nil)))

(ert-deftest smash-iterate ()
  (should (equal (%stream->list (%take 5 (%iterate '1+ 1)))
                 '(1 2 3 4 5))))

(ert-deftest smash-infinite-primes ()
  (defun divisible? (x y)
    (= (% x y) 0))

  (defun prime-sieve (xs)
    (%cons
     (%car xs)
     (prime-sieve (%filter
                   (lambda (x)
                     (not (divisible? x (%car xs))))
                   (%cdr xs)))))

  (let ((primes (prime-sieve (%iterate '1+ 2))))
    (should (equal (%stream->list (%take 8 primes))
                   '(2 3 5 7 11 13 17 19)))))

(provide 'smash-test)

;;; test-smash.el ends here
