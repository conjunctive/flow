;;; flow.el --- filtered process chains -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: process filter chain
;; Version: 0.0.2
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
;;         :log "./log-step1"
;;         :cwd "~"
;;         :command "sleep 1 && echo \"one\""
;;         :regex "one")
;;   (step :name "step2"
;;         :log "./log-step2"
;;         :cwd "~"
;;         :command (list "sleep 1"
;;                        "&& echo \"two\"")
;;         :regex "two")
;;   (step :name "step3"
;;         :log "./log-step3"
;;         :cwd "~"
;;         :command (list "sleep 1"
;;                        "&& echo \"three\"")
;;         :regex "\\(three\\|four\\)")
;;   (message result))
;;
;; (sleep-three)

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defun step-filter (regex action cwd &optional log)
  (lexical-let ((regex regex)
                (action action)
                (log-file (when log (concat (file-name-as-directory cwd) log)))
                (default-directory cwd)
                (stepped-p nil))
    (if (and (stringp log-file)
             (file-exists-p (file-name-directory log-file)))
        (lambda (proc str)
          (interactive)
          (write-region str nil log-file t 4096)
          (save-match-data
            (when (and (null stepped-p)
                       (string-match regex str))
              (prog1 (setq stepped-p t)
                (funcall action (match-string 1 str))))))
      (lambda (proc str)
        (interactive)
        (when (and (null stepped-p)
                   (string-match-p regex str))
          (prog1 (setq stepped-p t)
            (funcall action)))))))

(cl-defun step (&key name command regex action (log nil) (cwd default-directory))
  (let ((default-directory cwd)
        (cmd (if (stringp command) command
               (cl-reduce (lambda (v acc) (concat v " " acc)) command))))
    (set-process-filter (start-process-shell-command name nil cmd)
                        (step-filter regex action cwd log))))

;;;###autoload
(defmacro flow (&rest steps)
  `(lambda ()
     ,(cl-reduce (lambda (stepp acc)
                   (append stepp (list :action `(lambda (&optional result) ,acc))))
                 steps
                 :from-end t)))

(defmacro deflow (name &optional docstring &rest body)
  (declare (doc-string 2) (indent 1))
  (or name (error "Cannot define '%s' as a function" name))
  `(defun ,name ()
     ,docstring
     (interactive)
     ,`(funcall (flow ,@body))))

(provide 'flow)

;;; flow.el ends here
