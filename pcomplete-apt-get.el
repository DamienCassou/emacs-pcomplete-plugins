;;; pcomplete-apt-get.el --- functions for dealing with apt-get completions

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Package: pcomplete

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These functions provide completion rules for the `apt-get' tool.

;;; Code:

(require 'pcomplete)

(defgroup pcmpl-apt-get nil
  "Functions for dealing with APT-GET completions."
  :group 'pcomplete)

;; User Variables:

(defcustom pcmpl-apt-get-binary (or (executable-find "apt-get") "apt-get")
  "The full path of the 'apt-get' binary."
  :type 'file
  :group 'pcmpl-apt-get)

;; Functions:

;;;###autoload
(defun pcomplete/apt-get ()
  "Completion rules for the `apt-get' command."
  (let ((pcomplete-help "(apt-get)Invoking APT-GET"))
    (pcomplete-here* (pcmpl-apt-get-commands))
    (cond ((or (pcomplete-test "remove")
	       (pcomplete-test "autoremove")
	       (pcomplete-test "purge"))
	   (setq pcomplete-help "(apt-get)Removing packages")
	   (while (pcomplete-here (pcmpl-apt-get-installed-packages)))))))

(defun pcmpl-apt-get-commands ()
  "Return a list of available APT-GET commands."
  (pcomplete-uniqify-list
   (list "install" "remove" "autoremove" "purge" "source" "build" "dep" "changelog" "download")))

(defun pcmpl-apt-get-installed-packages ()
  "Return a list of all packages installed through apt."
  (split-string
   (shell-command-to-string
    (concat "grep -A 1 \"Package: $1\" /var/lib/dpkg/status | "
	  "grep -B 1 -Ee \"ok installed|half-installed|unpacked|half-configured|config-files\" -Ee \"^Essential: yes\" | "
	  "grep \"Package: $1\" | cut -d\\  -f2"))))

(provide 'pcomplete-apt-get)

;;; pcomplete-apt-get.el ends here
