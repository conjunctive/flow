;;; flow.el --- filtered process chains -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: process filter chain
;; Version: 0.0.1
;; URL: https://github.com/conjunctive/flow
;; Package-Requires: ((emacs "26") cl-lib)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (deflow sleep-three
;;   "Sleep for three seconds."
;;   (step :name "step1"
;;         :command "sleep 1 && echo \"one\""
;;         :regex "one")
;;   (step :name "step2"
;;         :command "sleep 1 && echo \"two\""
;;         :regex "two")
;;   (step :name "step3"
;;         :command "sleep 1 && echo \"three\""
;;         :regex "three")
;;   (print "done"))
;;
;; (sleep-three)

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defun step-filter (regex action)
  (lexical-let ((regex regex)
                (action action))
    (lambda (proc str)
      (when (string-match-p regex str)
        (funcall action)))))

(cl-defun step (&key name command regex action)
  (let ((proc (start-process-shell-command name nil command)))
    (set-process-filter proc (step-filter regex action))))

(defmacro flow (&rest steps)
  `(lambda ()
     ,(cl-reduce (lambda (stepp acc)
                   (append stepp (list :action `(lambda () ,acc))))
                 steps
                 :from-end t)))

(defmacro deflow (name &optional docstring &rest body)
  (declare (doc-string 2) (indent 1))
  (or name (error "Cannot define '%s' as a function" name))
  `(defun ,name ()
     ,docstring
     (interactive)
     ,`(funcall (flow ,@body))))

(provide 'deflow)

;;; flow.el ends here
