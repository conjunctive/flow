;;; flow.el --- filtered process chains -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: process filter chain
;; Version: 1.0.1
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

;; (defun started (step)
;;   "Function to run when the process has started.
;; STEP is the current step being invoked."
;;   (message (concat "Started: " (gethash :name step))))
;;
;; (defun finished (step &optional result)
;;   "Function to run when the regular expression has been matched on the process's output.
;; STEP is the current step being invoked.
;; RESULT is the matched value of the first parenthesized expression in the regular expression.
;; For example, the RESULT of the following regular expression would be \"bc\".
;; (string= \"bc\"
;;          (let ((string \"abcd\"))
;;            (when (string-match \"a\\(.?*\\)d\" string)
;;              (match-string 1 string))))"
;;   (message (concat "Finished: " (gethash :name step))))
;;
;; (defun cleanup (step &optional result)
;;   "Function to run when the last step has finished.
;; Deletes any remaining processes and all of the log files.
;; STEP is the current step being invoked.
;; RESULT is the matched value of the first parenthesized expression in the regular expression."
;;   (message (concat "Result: " result))
;;   (dolist (proc (processes step))
;;     (when proc
;;       (prog1 (message (concat "Deleting process: " (process-name proc)))
;;         (delete-process proc))))
;;   (dolist (log-file (log-files step))
;;     (when (and (stringp log-file)
;;                (file-exists-p log-file))
;;       (prog1 (message (concat "Deleting log file: " log-file))
;;         (delete-file log-file)))))
;;
;; (defun sleep-nine-seconds ()
;;   "Sleep for nine seconds."
;;   (interactive)
;;   (flow (step :name "step1"
;;               :log "log-step1"
;;               :cwd "~"
;;               :command "sleep 3 && echo \"one\""
;;               :pre 'started
;;               :regex "one"
;;               :post 'finished)
;;         (step :name "step2"
;;               :log "log-step2"
;;               :cwd "~"
;;               :command (list "sleep 3"
;;                              "&& echo \"two\"")
;;               :pre 'started
;;               :regex "two"
;;               :post 'finished)
;;         (step :name "step3"
;;               :log "log-step3"
;;               :cwd "~"
;;               :command (list "sleep 3"
;;                              "&& echo \"three\"")
;;               :pre 'started
;;               :regex "\\(three\\|four\\)"
;;               :post 'cleanup)))
;;
;; (sleep-nine-seconds)

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defun step-filter (step)
  (lexical-let* ((step step)
                 (regex (gethash :regex step))
                 (action (gethash :action step))
                 (pre (gethash :pre step))
                 (post (gethash :post step))
                 (cwd (gethash :cwd step))
                 (log (gethash :log step))
                 (log-file (when log (concat (file-name-as-directory cwd) log)))
                 (default-directory cwd)
                 (stepped-p nil))
    (if (and (stringp log-file)
             (file-exists-p (file-name-directory log-file)))
        (lambda (proc str)
          (interactive)
          (write-region str nil log-file t 1)
          (save-match-data
            (when (and (null stepped-p)
                       (string-match regex str))
              (setq stepped-p t)
              (when (functionp post)
                (funcall post step (match-string 1 str)))
              (when (hash-table-p action)
                (invoke action)))))
      (lambda (proc str)
        (interactive)
        (save-match-data
          (when (and (null stepped-p)
                     (string-match regex str))
            (setq stepped-p t)
            (when (functionp post)
              (funcall post step (match-string 1 str)))
            (when (hash-table-p action)
              (invoke action))))))))

(cl-defun step (&key name command regex
                     (action nil) (caller nil)
                     (pre nil) (post nil)
                     (log nil) (cwd default-directory))
  (let ((step (make-hash-table :size 9 :test 'equal)))
    (prog1 step
      (puthash :name name step)
      (puthash :log log step)
      (puthash :cwd cwd step)
      (puthash :command command step)
      (puthash :regex regex step)
      (puthash :action action step)
      (puthash :caller caller step)
      (puthash :pre pre step)
      (puthash :post post step))))

(defun retrace (step)
  "Create a shallow copy of a STEP where :action and :caller are nullified."
  (let ((s (copy-hash-table step)))
    (prog1 s
      (remhash :action s)
      (remhash :caller s))))

(defun invoke (step)
  (let* ((command (gethash :command step))
         (pre (gethash :pre step))
         (default-directory (gethash :cwd step)))
    (prog1 step
      (when (functionp pre)
        (funcall pre step))
      (set-process-filter
       (start-process-shell-command (gethash :name step)
                                    nil
                                    (if (stringp command)
                                        command
                                      (cl-reduce (lambda (v acc) (concat v " " acc))
                                                 command)))
       (step-filter step)))))

(defun leftmost (step)
  (cl-labels ((f (s)
                 (if-let ((c (gethash :caller s)))
                     (f c)
                   s)))
    (f step)))

(defun all (step)
  (cl-labels ((f (acc s)
                 (if-let ((a (gethash :action s)))
                     (cons s (f acc a))
                   (cons s nil))))
    (f (list)
       (leftmost step))))

(defun log-files (step)
  (mapcar (lambda (s)
            (when-let ((log (gethash :log s)))
              (concat (file-name-as-directory (gethash :cwd s))
                      log)))
          (all step)))

(defun processes (step)
  (mapcar (lambda (s)
            (get-process (gethash :name s)))
          (all step)))

;;;###autoload
(defun flow (&rest steps)
  (invoke
   (cl-reduce (lambda (step path)
                (let ((s (retrace step)))
                  (prog1 s
                    (puthash :caller s path)
                    (puthash :action path s))))
              steps
              :from-end t)))

(provide 'flow)

;;; flow.el ends here
