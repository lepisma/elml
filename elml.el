;;; elml.el --- Emacs lisp html generator -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "26"))
;; URL: https://github.com/lepisma/elml

;;; Commentary:

;; Emacs lisp html generator
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom elml-indent 2
  "Indent value for pprinting.")

(defvar elml-solo-tags '(:img :br :hr)
  "Tags which are supposed to be used without closing forms.")

(defun elml-key-name (key)
  (cl-typecase key
    (keyword (string-remove-prefix ":" (symbol-name key)))
    (t (format "%s" key))))

(defun chunkify (items n)
  ;; TODO: Looks too clumsy
  (reverse
   (mapcar #'reverse
           (cl-reduce (lambda (acc it)
                        (if (= n (length (car acc)))
                            (cons (list it) acc)
                          (cons (cons it (car acc)) (cdr acc))))
                      items :initial-value '(())))))

(defun elml-format-attrs-pair (pair)
  (let ((key (car pair))
        (val (cadr pair)))
    (format "%s=\"%s\"" (elml-key-name key) val)))

(defun elml-format-attrs (attrs)
  "Format attrs plist vector to go in html tags.
TODO: Handle solo attributes."
  (if (or (null attrs) (zerop (length attrs)))
      ""
    (let ((pairs (chunkify attrs 2)))
      ;; We add a padding here to have a separation with tag name
      (format " %s" (string-join (mapcar #'elml-format-attrs-pair pairs) " ")))))

(defun elml-solo-tag? (tag-form)
  "Tell if the given TAG-FORM is a solo html tag type."
  (member (car tag-form) elml-solo-tags))

(defun elml-tag-name (tag-form)
  (elml-key-name (car tag-form)))

(defun elml-parse-attrs (tag-form)
  "Parse attribute form. If present, attributes are represented
as a vector at 1th index of the TAG-FORM. An example is:
(:tag [:key1 value1 :key2 value2] ...)"
  (when (and (< 1 (length tag-form))
             (vectorp (second tag-form)))
    (if (oddp (length (second tag-form)))
        (error "Incorrect attribute count for %s" tag-form)
      (second tag-form))))

(defun elml-parse-content (tag-form)
  (unless (elml-solo-tag? tag-form)
    (cl-remove-if #'vectorp (cdr tag-form))))

(defun elml-tag-to-string (tag-form indent)
  (let ((tag-name (elml-tag-name tag-form))
        (attrs-string (elml-format-attrs (elml-parse-attrs tag-form)))
        (content (elml-parse-content tag-form))
        (pad (make-string indent ?\s)))
    (if (elml-solo-tag? tag-form)
        (format "%s<%s%s />\n" pad tag-name attrs-string)
      (format "%s<%s%s>\n%s%s</%s>\n"
              pad tag-name attrs-string
              (string-join (mapcar (lambda (form) (elml-to-string form (+ elml-indent indent))) content))
              pad tag-name))))

(defun elml-to-string (form &optional indent)
  (if (listp form)
      (elml-tag-to-string form (or indent 0))
    (format "%s%s\n" (make-string (or indent 0) ?\s) form)))

(defmacro elml (form)
  `(elml-to-string ',form))

(provide 'elml)

;;; elml.el ends here
