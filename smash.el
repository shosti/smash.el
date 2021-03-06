;;; smash.el --- Lazy streams for Elisp -*- lexical-binding: t -*-

;; Copyright © 2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>
;; URL: http://github.com/shosti/smash.el
;; Version: 0.0.0
;; Created: 9 March 2014
;; Keywords: streams, lazy
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A super-simple lazy streams library for Elisp.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;;;;;;;;;;;
;; Macros ;;
;;;;;;;;;;;;

(defmacro %with-stream (xs &rest body)
  "Coerce XS to a lazy stream and execute BODY.

The variable XS is intentionally captured; it must be the same
variable inside and outside of BODY (although it does not need to
be called XS)."
  `(when ,xs
     (let ((,xs (%ensure-stream ,xs)))
       ,@body)))

(put '%with-stream 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;
;; Primitives ;;
;;;;;;;;;;;;;;;;

;;;###autoload
(defun %memo (expr)
  "Memoize EXPR.

EXPR must be a function of no arguments."
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if already-run?
          result
        (progn (setq result (funcall expr)
                     already-run? t)
               result)))))

;;;###autoload
(defmacro %delay (expr)
  "Delay execution of EXPR."
  `(%memo (lambda () ,expr)))

;;;###autoload
(defun %force (thunk)
  "Force execution of a delayed THUNK."
  (funcall thunk))

;;;###autoload
(defmacro %cons (x y)
  "Create a new lazy cons pair from X and Y."
  `(cons 'lazy-cons (cons ,x (%delay ,y))))

;;;###autoload
(defun %car (xs)
  "Return the car of lazy stream XS."
  (cadr xs))

;;;###autoload
(defun %cdr (xs)
  "Return the cdr of lazy stream XS."
  (when xs
    (%force (cddr xs))))

;;;###autoload
(defun %stream? (xs)
  "Return t if XS is a lazy stream."
  (and (consp xs) (eq (car xs) 'lazy-cons)))

;;;;;;;;;;;;;;;;;;;;;
;; List conversion ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun %stream->list (xs)
  "Convert lazy stream XS to a concrete list.

Eagerly evaluated; do not use on infinite streams."
  (when xs
    (cons (%car xs) (%stream->list (%cdr xs)))))

;;;###autoload
(defun %list->stream (list)
  "Return a lazy stream of the elements from list LIST."
  (when list
    (%cons (car list) (%list->stream (cdr list)))))

;;;###autoload
(defun %stream (&rest items)
  "Return a lazy stream of ITEMS."
  (%list->stream items))

;;;###autoload
(defun %ensure-stream (xs)
  "Make sure that XS is a lazy stream if possible."
  (cond ((%stream? xs) xs)
        ((listp xs) (%list->stream xs))
        (t (error "Stream or list required"))))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;;;###autoload
(defun %nth (n xs)
  "Return the Nth element of lazy stream XS."
  (%with-stream xs
    (while (and xs (> n 0))
      (setq xs (%cdr xs)
            n (1- n)))
    (%car xs)))

;;;###autoload
(defun %take (n xs)
  "Return a lazy stream of the first N elements of lazy stream XS."
  (%with-stream xs
    (unless (<= n 0)
      (%cons (%car xs) (%take (1- n) (%cdr xs))))))

;;;###autoload
(defun %take-while (pred xs)
  "Return a lazy stream of the first elements for which PRED is true from XS."
  (%with-stream xs
    (when (and xs (funcall pred (%car xs)))
      (%cons (%car xs) (%take-while pred (%cdr xs))))))

;;;###autoload
(defun %drop (n xs)
  (while (and xs (> n 0))
    (setq xs (%cdr xs)
          n (1- n)))
  xs)

;;;###autoload
(defun %drop-while (pred xs)
  (while (and xs (funcall pred (%car xs)))
    (setq xs (%cdr xs)))
  xs)

;;;###autoload
(defun %map (fn xs)
  "Map FN over lazy stream XS."
  (%with-stream xs
    (%cons (funcall fn (%car xs))
           (%map fn (%cdr xs)))))

;;;###autoload
(defun %reduce (fn xs)
  "Reduce two-argument FN across lazy stream XS."
  (%with-stream xs
    (%reduce-from fn (%car xs) (%cdr xs))))

;;;###autoload
(defun %reduce-from (fn initial-value xs)
  "Reduce two-argument FN, starting with INITIAL-VALUE, across lazy stream XS.

Eagerly evaluated; do not use on infinite streams."
  (let ((acc initial-value))
    (%with-stream xs
      (while xs
        (setq acc (funcall fn acc (%car xs))
              xs (%cdr xs))))
    acc))

;;;###autoload
(defun %filter (pred xs)
  "Return a lazy stream of elements for which PRED is true from lazy stream XS."
  (%with-stream xs
    (let ((x (%car xs)))
      (if (funcall pred x)
          (%cons x (%filter pred (%cdr xs)))
        (%filter pred (%cdr xs))))))

;;;###autoload
(defun %contains? (xs x)
  "Return t if lazy stream XS contains element X.

Eagerly evaluated; do not use on infinite streams."
  (%with-stream xs
    (catch 'return
      (while xs
        (if (equal (%car xs) x)
            (throw 'return t)
          (setq xs (%cdr xs)))))))

;;;###autoload
(defun %any? (pred xs)
  "Return t if PRED is true for any element of lazy stream XS.

Eagerly evaluated; do not use on infinite streams."
  (%with-stream xs
    (catch 'return
      (while xs
        (if (funcall pred (%car xs))
            (throw 'return t)
          (setq xs (%cdr xs)))))))

;;;###autoload
(defun %iterate (fn x)
  "Return an infinite lazy stream from repeatedly applying FN, starting with X."
  (%cons x (%iterate fn (funcall fn x))))

(provide 'smash)

;;; smash.el ends here
