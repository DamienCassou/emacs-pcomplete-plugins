;;; pcmpl-darcs.el --- functions for dealing with darcs completions

;; Copyright (C) 2004 Sacha Chua

;; Author: Sacha Chua <sacha.free.net.ph>
;; Based on Jeremy Shaw's pcmpl-tla.el

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; These functions provide completion rules for the `darcs'
;; revision control system.

;;; Code:

(provide 'pcmpl-darcs)

(require 'pcomplete)
(require 'executable)

;; User Variables:

(defgroup pcmpl-darcs nil
  "Functions for dealing with darcs completions."
  :group 'pcomplete)

(defcustom pcmpl-darcs-binary (or (executable-find "darcs") "darcs")
  "*The full path of the 'darcs' binary."
  :type 'file
  :group 'pcmpl-darcs)

;; Functions:

(defun pcmpl-darcs-email ()
  (list user-mail-address))

(defvar pcmpl-darcs-commands nil)
(defvar pcmpl-darcs-command-tree nil)

;;;###autoload
(defun pcomplete/darcs ()
  "Completion rules for the `darcs' command."
  (let ((pcomplete-help "(darcs)"))
    (when (pcomplete-match "^-" 0)
      (pcomplete-here* '("--help")))
    (pcomplete-here* pcmpl-darcs-commands)
    (while (pcomplete-match "^-" 0)
      (pcmpl-darcs-complete-opt (pcomplete-arg 'first 1)))
    ;; TODO: pcomplete other commands intelligently.
    (while (pcomplete-here (pcomplete-entries)))))

(defvar pcmpl-darcs-completion-types
  '(("DIRECTORY" . (pcomplete-dirs))
    ("TAGNAME" . (pcmpl-darcs-tags))
    ("PATCHNAME" . (pcmpl-darcs-patches))
    ("COMMAND" . (pcomplete-executables))
    ("EMAIL" . (pcmpl-darcs-email))
    ("FROM" . (pcmpl-darcs-email))
    ("KEYS" . (pcomplete-entries))
    ("URL" . (pcomplete-entries))
    ("REPONAME" . (pcomplete-entries))
    ("USERNAME" . (pcmpl-darcs-users))
    ("IDFILE" . (pcomplete-entries))
    ("KEYID" . (pcmpl-darcs-keyids))
    ("FILE" . (pcomplete-entries)))
  "Alist of (TYPE . FUNCTION) completions.")

(defun pcmpl-darcs-complete-opt (command)
  "Pcomplete the options for COMMAND."
  (add-to-list 'pcomplete-suffix-list ?=)
  (let (short-option)
    (if (pcomplete-match "^--" 0)
        (if (pcomplete-match "^--\\([^= \t\n\f]*\\)\\'" 0)
            (pcomplete-here*
             (mapcar 'cadr (elt (assoc command pcmpl-darcs-command-tree) 2))))
      (pcomplete-here*
       (mapcar 'car (elt (assoc command pcmpl-darcs-command-tree) 2)))
      (setq short-option t))
    (mapcar
     (lambda (option)
       (when (and (elt option 2) ;; has an argument
                  ;; and the option matches either the long or short version
                  (or (and (elt option 0)
                           (pcomplete-match
                            (concat "\\`" (elt option 0)) 1))
                      (and (elt option 1) (pcomplete-match
                                           (concat "\\`" (elt option 1)
                                                   "\\(.*\\)") 0))))
         (pcomplete--here
          (cdr (assoc (elt option 2) pcmpl-darcs-completion-types))
          (unless short-option
            (pcomplete-match-string 1 0))
          nil
          t)))
     (elt (assoc command pcmpl-darcs-command-tree) 2))))

(defun pcmpl-darcs-keyids ()
  "Return key IDs from GPG."
  (with-temp-buffer
    (call-process "gpg" nil t nil "--list-keys")
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "/\\(.+?)\\s-" nil t)
        (setq results (cons (match-string 1) results)))
      results)))

(defun pcmpl-darcs-users ()
  "Return users from /etc/passwd."
  (with-temp-buffer
    (insert-file-contents-literally "/etc/passwd")
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "^[^:]+" nil t)
        (setq results (cons (match-string 0) results)))
      results)))

(defun pcmpl-darcs-repository-names ()
  "Return a list of repositories."
  (pcomplete-entries))

(defun pcmpl-darcs-tags ()
  "Return a list of all the tags."
  (let (results)
    (with-temp-buffer
      (call-process pcmpl-darcs-binary nil t nil "changes" "--no-summary")
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+tagged\\s-+\\(.+\\)" nil t)
        (setq results (cons (match-string 1) results))))
    results))

(defun pcmpl-darcs-patches ()
  "Return a list of all the patch names."
  ;; TODO:
  (let (results)
    (with-temp-buffer
      (call-process pcmpl-darcs-binary nil t nil "changes" "--no-summary")
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+\\*\\s-+\\(.+\\)" nil t)
        (setq results (cons (match-string 1) results))))
    results))

(defun pcmpl-darcs-commands ()
  "Return a list of available darcs commands."
  (with-temp-buffer
    (call-process pcmpl-darcs-binary nil t nil "help")
    (goto-char (point-min))
    (let (cmds)
      (while (re-search-forward "^\\s-+\\([-a-z]+\\)" nil t)
        (setq cmds (cons (match-string 1) cmds)))
      (pcomplete-uniqify-list cmds))))

(defun pcmpl-darcs-find-command-usage (command)
  "Return an alist of options for COMMAND.
Entries are of the form (COMMAND DESCRIPTION (OPTIONS...)) where
each option is a list of the form (SHORT-OPTION LONG-OPTION
ARG-COMPLETION DESCRIPTION). If the option requires an argument,
ARG-COMPLETION is a string specifying the format."
  (with-temp-buffer
    (call-process pcmpl-darcs-binary nil t nil command "--help")
    (goto-char (point-min))
    (forward-line)
    (let (results
          (regexp
           (concat
            "^\\s-+\\(\\-[^-]\\)?[^-]+" ;; short option
            "\\(--[^= \t\n]+=?\\)\\([A-Z]+\\)?\\s-+\\(.+\\)")) ;; long option
          (description (buffer-substring (line-beginning-position)
                                         (line-end-position))))
      (while (re-search-forward regexp nil t)
        (setq results (cons (list (match-string 1)
                                  (match-string 2)
                                  (match-string 3)
                                  (match-string 4))
                            results)))
      (list command description results))))

(defun pcmpl-darcs-initialize ()
  "Make a list of all the darcs commands and their options."
  (setq pcmpl-darcs-commands (pcmpl-darcs-commands))
  (setq pcmpl-darcs-command-tree
        (mapcar 'pcmpl-darcs-find-command-usage pcmpl-darcs-commands)))

(pcmpl-darcs-initialize)

;;; pcmpl-darcs.el ends here
