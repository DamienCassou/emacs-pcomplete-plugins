;;; pcmpl-arch.el --- functions for dealing with arch/tla completions

;; Copyright (C) 2004 Sacha Chua

;; Author: Sacha Chua <sacha.free.net.ph>

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

;; These functions provide completion rules for the `tla' tool of the
;; arch revision control system.

;;; Code:

(provide 'pcmpl-arch)

(require 'pcomplete)
(require 'executable)

(defgroup pcmpl-arch nil
  "Functions for dealing with ARCH completions"
  :group 'pcomplete)

;; User Variables:

(defcustom pcmpl-arch-binary (or (executable-find "tla") "tla")
  "*The full path of the 'arch' binary."
  :type 'file
  :group 'pcmpl-arch)

;; Functions:

;;;###autoload
(defun pcomplete/tla ()
  "Completion rules for the `arch' command."
  (setq pcomplete-termination-string " ")
  (pcomplete-opt "hHVAdrscDf")
  (pcomplete-here* (pcmpl-arch-commands))
  (cond
;;                         tla sub-commands
;;                         ----------------
;; * help
;;                         help : provide help with arch
;; * User Commands
;;                        my-id : print or change your id
   ((pcomplete-test "my-default-archive")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--errname" "--uid"))
        (pcomplete-opt "e(pcomplete-executables)ds"))
      (cond
       ((pcomplete-test "--errname" 1)
        (pcomplete-here (pcomplete-executables))))))
;;           my-default-archive : print or change your default archive
   ((pcomplete-test "my-default-archive")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--errname" "--delete" "--silent"))
        (pcomplete-opt "A(pcmpl-arch-archive)e(pcomplete-executables)ds"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--errname" 1)
        (pcomplete-here (pcomplete-executables)))))
    (pcomplete-here (pcmpl-arch-archives)))
;;             register-archive : change an archive location registration
   ((pcomplete-test "register-archive")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--force" "--delete"))
        (pcomplete-opt "fd")))
    (pcomplete-here (pcmpl-arch-archives))
    (pcomplete-here (pcomplete-dirs)))
;;              whereis-archive : print an archive location registration
   ((pcomplete-test "archives")
    (pcomplete-here (pcmpl-arch-archives)))
;;                     archives : Report registered archives and their locations.
   ((pcomplete-test "archives")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--names" "--exclude-remote"))
        (pcomplete-opt "nR"))))
;; * Project Tree Commands
;;                    init-tree : initialize a new project tree
   ((pcomplete-test "init-tree")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--nested"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-versions)))
;;                    tree-root : find and print the root of a project tree
   ((pcomplete-test "tree-root"
    (while (pcomplete-match "^-" 0)
      (pcomplete-here '("--accurate" "--silent"))))
    (pcomplete-here (pcomplete-dirs)))
;;                 tree-version : print the default version for a project tree
   ((pcomplete-test "tree-version")
    (pcomplete-here (pcomplete-dirs)))
;;             set-tree-version : set the default version for a project tree
   ((pcomplete-test "set-tree-version")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-versions)))
;;                         undo : undo and save changes in a project tree
   ((pcomplete-test "undo")
    (let (seen-end)
      (while (pcomplete-match "^-" 0)
        (if (pcomplete-match "^--" 0)
            (pcomplete-here '("--archive" "--dir" "--output" "--no-output"
                              "--forward" "--quiet"))
          (pcomplete-opt "Nkqd(pcomplete-dirs)"))
        (cond
         ((pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives)))
         ((or (pcomplete-test "--dir" 1)
              (pcomplete-test "--output" 1))
          (pcomplete-here (pcomplete-dirs)))))
      (pcomplete-here (pcmpl-arch-revisions))))
;;                         redo : redo changes in project tree
   ((pcomplete-test "redo")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--forward" "--keep" "--quiet" "--dir"))
        (pcomplete-opt "Nkqd(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((or (pcomplete-test "--dir" 1)
            (pcomplete-test "--output" 1))
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-dirs))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                      changes : report about local changes in a project tree
   ((pcomplete-test "file-diffs")
    (let (seen-end)
      (while (pcomplete-match "^-" 0)
        (if (pcomplete-match "^--" 0)
            (pcomplete-here '("--archive" "--dir" "--output" "--verbose" "--quiet" "--diffs"
                              "--keep" "--link"))
          (pcomplete-opt "A(pcmpl-arch-archives)dovqk"))
        (cond
         ((pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives)))
         ((or (pcomplete-test "--dir" 1)
              (pcomplete-test "--output" 1))
          (pcomplete-here (pcomplete-dirs)))))
      (pcomplete-here (pcmpl-arch-revisions))
      (pcomplete-here '("--"))
      (while (pcomplete-here (pcmpl-arch-revisions)))))
;;                   file-diffs : show local changes to a file
   ((pcomplete-test "file-diffs")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--new-file"))
        (pcomplete-opt "A(pcmpl-arch-archives)N"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcomplete-entries))
    (pcomplete-here (pcomplete-arch-revisions)))
;; * Project Tree Inventory Commands

;;                    inventory : inventory a source tree
   ((pcomplete-test "inventory")
    (let (seen-end)
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--directories" "--files"
                            "--both" "--kind"
                            "--all" "--nested"
                            "--ids" "--untagged"
                            "--explicit" "--implicit"
                            "--tagline" "--names")))
      (pcomplete-here (pcomplete-dirs)))))
;;                    tree-lint : audit a source tree
   ((pcomplete-test "tree-lint")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--broken-symlinks" "--unrecognized-files"
                            "--untagged-files" "--missing-files"
                            "--duplicate-ids" "--strict"))
        (pcomplete-opt "sutmd"))
      (pcomplete-here (pcomplete-dirs))))
;;                           id : report the inventory id for a file
   ((pcomplete-test "id-tagging-method")
    (while (pcomplete-match "^-" 0)
      (pcomplete-here '("--implicit" "--tagline" "--explicit" "--names" "--silent")))
    (pcomplete-here (pcomplete-entries)))
;;            id-tagging-method : print or change a project tree id tagging method
   ((pcomplete-test "id-tagging-method")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--strict"))
        (pcomplete-opt "d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here* '("names" "implicit" "explicit" "tagline")))
;;                       add-id : add an explicit inventory id
;;                          add : (alias for add-id)
   ((or (pcomplete-test "add-id")
        (pcomplete-test "add"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--id"))
        (pcomplete-opt "i")))
    (while (pcomplete-here (pcomplete-entries))))
   
;;                    delete-id : remove an explicit inventory id
;;                       delete : (alias for delete-id)
;;                           rm : remove a file (or dir, or symlink) and its explicit inventory tag (if any)
   ((or (pcomplete-test "delete-id")
        (pcomplete-test "delete")
        (pcomplete-test "rm"))
    (while (pcomplete-here (pcomplete-entries))))
;;                      move-id : move an explicit inventory id
;;                         move : (alias for move-id)
   ((or
     (pcomplete-test "move-id")
     (pcomplete-test "move"))
    (pcomplete-here
     (pcomplete-entries nil (lambda (filename) (not (file-directory-p filename)))))
    (pcomplete-here
     (pcomplete-entries nil (lambda (filename) (not (file-directory-p filename))))))
;;                           mv : move a file (or dir, or symlink) and it's explicit inventory tag (if any)
   ((pcomplete-test "mv")
    (while (pcomplete-here (pcomplete-entries))))
;;             explicit-default : print or modify default ids
;;                   default-id : (alias for explicit-default)
   ((or (pcomplete-test "explicit-default")
        (pcomplete-test "default-id"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--delete" "--strong" "--weak" "--dont-care"))
        (pcomplete-opt "D(pcomplete-dirs)dsw"))
      (cond
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs))))))
;;          id-tagging-defaults : print the default =tagging-method contents
   ;; Nothing

;; * Patch Set Commands

;;                    changeset : compute a whole-tree changeset
;;                      mkpatch : (alias for changeset)
   ((or (pcomplete-test "changeset")
        (pcomplete-test "mkpatch"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--file-list"))
        (pcomplete-opt "NR")))
    (pcomplete-here (pcomplete-dirs))
    (pcomplete-here (pcomplete-dirs))
    (pcomplete-here (pcomplete-dirs))
    (while (pcomplete-here (pcomplete-entries))))
;;              apply-changeset : apply a whole-tree changeset
;;                      dopatch : (alias for apply-changeset)
   ((or (pcomplete-test "apply-changeset")
        (pcomplete-test "dopatch"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--forward" "--reverse"))
        (pcomplete-opt "NR")))
    (pcomplete-here (pcomplete-dirs))
    (pcomplete-here (pcomplete-dirs)))    
;;               show-changeset : generate a report from a changeset
   ((pcomplete-test "show-changeset")
    (if (pcomplete-match "^-" 0)
        (pcomplete-here '("--diffs"))
      (pcomplete-here (pcomplete-dirs))))

;; * Archive Transaction Commands

;;                 make-archive : create a new archive directory
   ((pcomplete-test "make-archive")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--mirror" "--mirror-from" "--listing" "--signed"))
        (pcomplete-opt "m(pcmpl-arch-archives)M(pcmpl-arch-archives)ls"))
      (cond
       ((or (pcomplete-test "--mirror" 1)
            (pcomplete-test "--mirror-from" 1))
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcomplete-dirs)))
;;                archive-setup : create new categories, branches and versions
   ((pcomplete-test "archive-setup")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--file" "--branches" "--cache"))
        (pcomplete-opt "A(pcmpl-arch-archives)f(pcomplete-entries)bc"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--file" 1)
        (pcomplete-here (pcomplete-entries)))))
    (setq pcomplete-suffix-list (cons ?- pcomplete-suffix-list))
    (setq pcomplete-termination-string "--")
    (pcomplete-here (nconc
                     (pcmpl-arch-categories)
                     (pcmpl-arch-all-branches))))
;;                make-category : create a new archive category
   ((pcomplete-test "make-category")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives))))))
;;                  make-branch : create a new archive branch
   ((pcomplete-test "make-version")
    (setq pcomplete-suffix-list (cons ?- pcomplete-suffix-list))
    (setq pcomplete-termination-string "--")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-categories)))
;;                 make-version : create a new archive version
   ((pcomplete-test "make-version")
    (setq pcomplete-suffix-list (cons ?- pcomplete-suffix-list))
    (setq pcomplete-termination-string "--")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-all-branches)))
;;                       import : archive a full-source base-0 revision
   ((pcomplete-test "import")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--log" "--summary"
                            "--log-message" "--setup"))
        (pcomplete-opt "A(pcmpl-arch-archives)dlsLS"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--log" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-archives)))
;;                       commit : archive a changeset-based revision
   ((pcomplete-test "commit")
    (let (seen-end)
      (while (and (not seen-end) (pcomplete-match "^-" 0))
        (if (pcomplete-match "^--" 0)
            (pcomplete-here '("--archive" "--dir" "--log" "--summary"
                              "--log-message" "--strict" "--seal"
                              "--fix" "--out-of-date-ok" "--file-list"))
          (pcomplete-opt "A(pcmpl-arch-archives)dlsL"))
        (if (pcomplete-test "--")
            (setq seen-end t)
          (cond
           ((pcomplete-test "--archive" 1)
            (pcomplete-here (pcmpl-arch-archives)))
           ((pcomplete-test "--dir" 1)
            (pcomplete-here (pcomplete-dirs)))
           ((pcomplete-test "--log" 1)
            (pcomplete-here (pcomplete-entries))))))
      (if (not seen-end)
          (pcomplete-here (pcmpl-arch-revisions))
        (while seen-end
          (pcomplete-here (pcomplete-entries))))))
;;                          get : construct a project tree for a revision
   ((pcomplete-test "get")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--cache" "--no-pristine" "--link"
                            "--library" "--sparse" "--non-sparse" "--silent"
                            "--no-greedy-add"))
        (pcomplete-opt "A(pcmpl-arch-archives)s"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--cache" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                get-changeset : retrieve a changeset from an archive
   ((pcomplete-test "get-changeset")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (when (pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                lock-revision : lock (or unlock) an archive revision
   ((pcomplete-test "lock-revision")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here
           '("--archive" "--unlock" "--break"))
        (pcomplete-opt "A(pcmpl-arch-archives)ub"))
      (when (pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;               archive-mirror : update an archive mirror
   ((pcomplete-test "archive-mirror")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here
           '("--no-cached" "--summary" "--cached-tags"))
        (pcomplete-opt "s")))
    (pcomplete-here (pcmpl-arch-archives))
    (pcomplete-here (pcmpl-arch-archives))
    (pcomplete-here (pcmpl-arch-revisions)))

;; * Archive Commands

;;                      abrowse : print an outline describing archive contents
   ((pcomplete-test "abrowse")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here
           '("--archive" "--reverse" "--summary" "--creator"
             "--datex" "--kind" "--full" "--desc"
             "--local-merges" "--foreign-merges" "--merges"
             "--categories" "--branches" "--versions"
             "--omit-empty" "--since" "--since-limits"
             "--snap" "--force"))
        (pcomplete-opt "A(pcmpl-arch-archives)rscDkf"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--since" 1)
        (pcomplete-here (pcomplete-entries)))
       ((pcomplete-test "--snap" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-archives)))
;;                      rbrowse : print an outline describing an archive's contents
   ((pcomplete-test "rbrowse")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here
           '("--all" "--since" "--snap" "--snap-force"
             "--patch-regex" "--archive" "--show-sealed"))
        (pcomplete-opt "frscDA(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--since" 1)
        (pcomplete-here (pcomplete-entries)))
       ((pcomplete-test "--snap" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-archives)))
;;                   categories : list the categories in an archive
   ((pcomplete-test "categories")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)r"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-archives)))   
;;                     branches : list the branches in an archive category
   ((pcomplete-test "branches")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)r"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-categories)))
;;                     versions : list the versions in an archive branch
   ((pcomplete-test "revisions")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--reverse"))
        (pcomplete-opt "A(pcmpl-arch-archives)r"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-branches)))
;;                    revisions : list the revisions in an archive version
   ((pcomplete-test "revisions")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--reverse" "--full" "--summary" "--creator" "--date"))
        (pcomplete-opt "A(pcmpl-arch-archives)rfscD"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-versions)))
;;                     ancestry : display the ancestory of a revision
   ((pcomplete-test "ancestry")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--merges" "--reverse" "--summary" "--creator" "--date"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)mrscD"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;               ancestry-graph : display the ancestory of a revision
   ((pcomplete-test "ancestry-graph")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--merges" "--reverse" "--immediate" "--previous"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)mrip"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;              cat-archive-log : print the contents of an archived log entry
   ((pcomplete-test "cat-archive-log")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--headers"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                     cacherev : cache a full source tree in an archive
   ((pcomplete-test "cacherev")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--cache"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--cache" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                   cachedrevs : list cached revisions in an archive
   ((pcomplete-test "cachedrevs")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-versions)))
;;                   uncacherev : remove a cached full source tree from an archive
   ((pcomplete-test "uncacherev")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcmpl-arch-revisions))
    (pcomplete-here (pcomplete-dirs))    )
;;            archive-meta-info : report meta-info from an archive
   ((pcomplete-test "archive-snapshot")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here '("http-blows" "name" "signed-archive")))
;;             archive-snapshot : update an archive snapshot
   ((pcomplete-test "archive-snapshot")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))))
    (pcomplete-here (pcomplete-dirs)))
;;              archive-version : list the archive-version in an archive
;;                archive-fixup : fix ancillary files (e.g. .listing files) in an archive
   ((or
     (pcomplete-test "archive-version")
     (pcomplete-test "archive-fixup"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives))))))

;; * Patch Log Commands

;;                     make-log : initialize a new log file entry
   ((pcomplete-test "make-log")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                 log-versions : list patch log versions in a project tree
   ((pcomplete-test "log-versions")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--archive" "--category" "--branch" "--vsn" "--reverse"))
        (pcomplete-opt "d(pcomplete-dirs)a(pcmpl-arch-archives)c(pcmpl-arch-categories)b(pcmpl-arch-branches)v(pcmpl-arch-versions)"))
      (cond
       ((pcomplete-test "--vsn" 1)
        (pcomplete-here (pcmpl-arch-versions)))
       ((pcomplete-test "--branch" 1)
        (pcomplete-here (pcmpl-arch-branches)))
       ((pcomplete-test "--category" 1)
        (pcomplete-here (pcmpl-arch-category)))
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;              add-log-version : add a patch log version to a project tree
   ((pcomplete-test "add-log-version")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;           remove-log-version : remove a version's patch log from a project tree
   ((pcomplete-test "remove-log-version")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
   ;;                         logs : list patch logs for a version in a project tree
   ((pcomplete-test "logs")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--reverse" "--summary" "--creator" "--date" "--local-merges" "--foreign-merges" "--merges" "--full"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)rscD"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                      cat-log : print the contents of a project tree log entry
   ((pcomplete-test "cat-log")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                    changelog : generate a ChangeLog from a patch log
   ((pcomplete-test "changelog")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--no-files" "--untagged" "--new-entry"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--new-entry" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                log-for-merge : generate a log entry body for a merge
   ((pcomplete-test "log-for-merge")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--reverse"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)rf"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions"))
      (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                       merges : report where two branches have been merged
   ((pcomplete-test "merges")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--reverse" "--full"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)rf"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
      (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                   new-merges : list tree patches new to a version
   ((pcomplete-test "new-merges")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--reverse"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)r"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
      (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;; * Multi-project Configuration Commands
;;                 build-config : instantiate a multi-project config
   ((pcomplete-test "build-config")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--no-pristines" "--link" "--library" "--sparse" "--release-id"))
        (pcomplete-opt "d(pcomplete-dirs)r"))
      (cond
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
      (pcomplete-here (pcomplete-entries)))
;;                   cat-config : output information about a multi-project config
   ((pcomplete-test "cat-config")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--output" "--force" "--snap"))
        (pcomplete-opt "d(pcomplete-dirs)o(pcomplete-entries)fs"))
      (cond
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs))))
      (pcomplete-here (pcomplete-entries))))
;; * Commands for Branching and Merging
;;                          tag : create a continuation revision (aka tag or branch)
   ((pcomplete-test "tag")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--log" "--no-cacherev" "--seal" "--fix" "--setup"))
        (pcomplete-opt "A(pcmpl-arch-archives)l(pcomplete-entries)S"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--log" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-revisions))
    (pcomplete-here (pcmpl-arch-versions)))
;;                       update : update a project tree to reflect recent archived changes
   ((pcomplete-test "update")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--forward" "--dest"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)N"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dest" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                       replay : apply revision changesets to a project tree
   ((pcomplete-test "replay")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--list" "--new" "--reverse" "--forward" "--dir" "--dest" "--skip-present"))
        (pcomplete-opt "A(pcmpl-arch-archives)Nd(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--list" 1)
        (pcomplete-here (pcomplete-entries)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dest" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
   ;;                   star-merge : merge mutually merged branches
   ((pcomplete-test "star-merge")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--changes" "--reference" "--forward" "--three-way" "--dir"))
        (pcomplete-opt "A(pcmpl-arch-archives)c(pcomplete-dirs)r(pcmpl-arch-versions)Ntd(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--changes" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--reference" 1)
        (pcomplete-here (pcmpl-arch-version)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-slurp-results "log-versions")))
;;                  apply-delta : Compute a changeset between any two trees or revisions and apply it to a project tree
   ((pcomplete-test "apply-delta")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--forward" "--cache" "--dir" "--dest"))
        (pcomplete-opt "A(pcmpl-arch-archives)Nd(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--cache" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dest" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    ;; TODO: Allow directories
    (pcomplete-here (pcmpl-arch-revisions))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                      missing : print patches missing from a project tree
   ((pcomplete-test "missing")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--reverse" "--summary" "--creator" "--date" "--full" "--names" "--merges" "--skip-present"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)rscDf"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (nconc (pcmpl-arch-revisions)
                           (pcmpl-arch-slurp-results "log-versions"))))
;;                  join-branch : construct a project tree for a version
   ((pcomplete-test "join-branch")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--dest"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dest" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-versions)))
;;                    sync-tree : unify a project tree's patch-log with a given revision
   ((pcomplete-test "sync-tree")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--dest"))
        (pcomplete-opt "A(pcmpl-arch-archives)d(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--dest" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                        delta : Compute a changeset (or diff) between any two trees or revisions
   ((pcomplete-test "delta")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--cache" "--diffs"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--cache" 1)
        (pcomplete-here (pcomplete-dirs)))))
    ;; TODO: Also allow dirs
    (pcomplete-here (pcmpl-arch-revisions))
    (pcomplete-here (pcmpl-arch-revisions))
    (pcomplete-here (pcomplete-dirs)))
;; * Local Cache Commands
;;                      changes : report about local changes in a project tree
   ((pcomplete-test "changes")
    (let (seen-end)
      (while (and (not seen-end) (pcomplete-match "^-" 0))
        (if (pcomplete-match "^--" 0)
            (pcomplete-here '("--archive" "--dir" "--output" "--verbose" "--quiet"
                              "--diffs" "--keep" "--link"))
          (pcomplete-opt "A(pcmpl-arch-archives)N"))
        (cond
         ((pcomplete-test "--")
          (setq seen-end t))
         ((pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives)))
         ((pcomplete-test "--dir" 1)
          (pcomplete-here (pcomplete-dirs)))
         ((pcomplete-test "--output" 1)
          (pcomplete-here (pcomplete-dirs)))))
      (if (not seen-end)
          (pcomplete-here (pcmpl-arch-revisions))
        (pcomplete-here (pcomplete-entries)))))
;;                   file-diffs : show local changes to a file
;;                    file-find : find given version of file
   ((or (pcomplete-test "file-diffs")
        (pcomplete-test "file-find"))
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--new-file"))
        (pcomplete-opt "A(pcmpl-arch-archives)N"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcomplete-entries))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                    pristines : list pristine trees in a project tree
   ((pcomplete-test "pristines")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--dir" "--unlocked" "--locked" "--reverse"))
        (pcomplete-opt "dulr"))
      (cond
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                lock-pristine : lock (or unlock) a pristine tree
   ((pcomplete-test "lock-pristine")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--unlock"))
        (pcomplete-opt "Adu"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                 add-pristine : ensure that a project tree has a particular pristine revision
   ((pcomplete-test "add-pristine")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir"))
        (pcomplete-opt "Ad"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                find-pristine : find and print the path to a pristine revision
   ((pcomplete-test "find-pristine")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--dir" "--unlocked"
                            "--locked" "--tree-only"
                            "--silent"))
        (pcomplete-opt "Adults"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--dir" 1)
        (pcomplete-here (pcomplete-dirs)))))
    (pcomplete-here (pcmpl-arch-revisions)))
;; * Revision Library Commands
;;          my-revision-library : print or change your revision library path
   ((pcomplete-test "my-revision-library")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--errname" "--delete" "--silent"
                            "--search" "--add"
                            "--search-only"
                            "--add-only"
                            "--first"))
        (pcomplete-opt "eds"))
      (if (pcomplete-test "--errname" 1)
          (pcomplete-here (pcomplete-executables))))
    (pcomplete-here (pcomplete-dirs)))
;;               library-config : configure parameters of a revision library
   ((pcomplete-test "library-find")
    (while (pcomplete-match "^-" 0)
      (pcomplete-here '("--greedy" "--non-greedy" "--sparse" "--non-sparse")))
    (pcomplete-here (pcomplete-dirs)))
;;                 library-find : find and print the location of a revision in the revision library
   ((pcomplete-test "library-find")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--errname" "--silent"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--errname" 1)
        (pcomplete-here (pcomplete-executables))))
    (pcomplete-here (pcmpl-arch-slurp-results "library-revisions" "-f"))))
;;                  library-add : add a revision to the revision library
   ((pcomplete-test "library-add")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--sparse" "--non-sparse" "--library" "--for-links"))
        (pcomplete-opt "A(pcmpl-arch-archives)sL(pcomplete-dirs)"))
      (cond
       ((pcomplete-test "--archive" 1)
        (pcomplete-here (pcmpl-arch-archives)))
       ((pcomplete-test "--library" 1)
        (pcomplete-here (pcomplete-dirs)))
       ((pcomplete-test "--for-links" 1)
        (pcomplete-here (pcomplete-entries)))))
    (pcomplete-here (pcmpl-arch-slurp-results "revisions" "-f")))
;;               library-remove : remove a revision from the revision library
   ((pcomplete-test "library-remove")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-slurp-results "library-revisions" "-f")))
;;             library-archives : list the archives in your revision library
;; Use default for library-archives
;;           library-categories : list the categories in your revision library
   ((pcomplete-test "library-categories")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-archives)))
;;             library-branches : list the branches in a library category
   ((pcomplete-test "library-branches")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-slurp-results "library-categories")))
;;             library-versions : list the versions in a library branch
   ((pcomplete-test "library-versions")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--reverse"))
        (pcomplete-opt "A(pcmpl-arch-archives)r"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-slurp-results "library-branches")))
;;            library-revisions : list the revisions in a library version
   ((pcomplete-test "library-revisions")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--reverse"
                            "--full" "--summary" "--creator" "--date"))
        (pcomplete-opt "A(pcmpl-arch-archives)rfscD"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-slurp-results "library-versions")))
;;                  library-log : output a log message from the revision library
   ((pcomplete-test "library-log")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--silent"))
        (pcomplete-opt "A(pcmpl-arch-archives)s"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcmpl-arch-revisions)))
;;                 library-file : find a file in a revision library
   ((pcomplete-test "library-file")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--archive" "--id" "--this"))
        (pcomplete-opt "A(pcmpl-arch-archives)"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives))))
    (pcomplete-here (pcomplete-entries))
    (pcomplete-here (pcmpl-arch-revisions)))
;; * Published Revisions Commands
;;                         grab : grab a published revision
   ((pcomplete-test "grab")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here '("--help" "--version"))
        (pcomplete-opt "hHV")))
    (pcomplete-here (pcomplete-entries)))
;; * Miscellaneous Scripting Support
;;           parse-package-name : parse a package name
   ((pcomplete-test "parse-package-name")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here* '("--archive"
                             "--arch"
                             "--non-arch"
                             "--category"
                             "--branch"
                             "--package"
                             "--vsn"
                             "--package-version"
                             "--lvl"
                             "--patch-level"))
        (pcomplete-opt "A(pcmpl-arch-archives)acbpvl"))
      (if (pcomplete-test "--archive" 1)
          (pcomplete-here (pcmpl-arch-archives)))))
;;          valid-package-name : test a package name for validity
   ((pcomplete-test "valid-package-name")
    (while (pcomplete-match "^-" 0)
      (if (pcomplete-match "^--" 0)
          (pcomplete-here* '("--errname"
                             "--archive"
                             "--no-archive"
                             "--category"
                             "--package"
                             "--vsn"
                             "--patch-level"
                             "--lvl"
                             "--tolerant"))
        (pcomplete-opt "ecpvlt"))))))

   
(defun pcmpl-arch-commands ()
  "Return a list of available ARCH commands."
  (with-temp-buffer
    (call-process pcmpl-arch-binary nil t nil "help")
    (goto-char (point-min))
    (let (cmds)
      (while (re-search-forward "^\\s-+\\([-a-z]+\\)\\s-+:" nil t)
	(setq cmds (cons (match-string 1) cmds)))
      (pcomplete-uniqify-list cmds))))

(defun pcmpl-arch-slurp-results (&rest options)
  "Return the results of a command as a list of lines."
  (with-temp-buffer
    (apply 'call-process pcmpl-arch-binary nil t nil options)
    (goto-char (point-min))
    (let (results)
      (while (not (eobp))
        (setq results (cons (buffer-substring (line-beginning-position)
                                              (line-end-position))
                             results))
        (forward-line))
      results)))

(defun pcmpl-arch-archives ()
  "Return a list of registered archives."
  (pcmpl-arch-slurp-results "archives" "-n"))

(defun pcmpl-arch-revisions ()
  "Return a list of registered revisions."
  (pcmpl-arch-slurp-results "revisions" "-f"))

(defun pcmpl-arch-categories ()
  "Return a list of registered categories."
  (pcmpl-arch-slurp-results "categories"))

(defun pcmpl-arch-all-branches ()
  "Return a list of all registered branches."
  (with-temp-buffer
    (call-process pcmpl-arch-binary nil t nil "abrowse" "--branches")
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "^\\s-+\\(.+--.+\\)" nil t)
        (setq results (cons (match-string 1) results)))
      results)))
        
(defun pcmpl-arch-branches ()
  "Return a list of registered branches."
  (pcmpl-arch-slurp-results "branches"))

;;(defun pcmpl-arch-modules ()
;;  "Return a list of available modules under ARCH."
;;  (with-temp-buffer
;;    (call-process pcmpl-arch-binary nil t nil "checkout" "-c")
;;    (goto-char (point-min))
;;     (let (entries)
;;       (while (re-search-forward "\\(\\S-+\\)$" nil t)
;; 	(setq entries (cons (match-string 1) entries)))
;;       (pcomplete-uniqify-list entries))))

(defun pcmpl-arch-entries (&optional opers)
  "Return the precious files for the current directory.
If OPERS is a list of characters or strings, return entries for which that
operation character applies, as displayed by 'tla inventory'."
  (let* ((arg (pcomplete-arg))
	 (dir (file-name-as-directory
	       (or (file-name-directory arg) "")))
	 (nondir (or (file-name-nondirectory arg) ""))
	 entries)
    (if opers
	(with-temp-buffer
	  (and dir (cd dir))
	  (call-process pcmpl-arch-binary nil t nil
			"inventory") ; "-l")
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.\\)\\s-+\\(.+\\)$" nil t)
	    (if (member (match-string 1) opers)
		(setq entries (cons (match-string 2) entries)))))
      (with-temp-buffer
        (call-process pcmpl-arch-binary nil t nil "inventory")
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((line (buffer-substring (line-beginning-position)
					 (line-end-position))))
            (setq entries (cons line entries)))
	  (forward-line))))
    (setq pcomplete-stub nondir)
    (pcomplete-uniqify-list entries)))


;;; pcmpl-arch.el ends here
