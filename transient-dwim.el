;;; transient-dwim.el --- Useful preset transient commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26.1") (transient "0.1.0"))
;; URL: https://github.com/conao3/transient-dwim.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Useful preset transient commands

;; To Use this package, simply add this to your init.el:
;;   (define-key global-map (kbd "M-=") 'transient-dwim-dispatch)


;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)

(defgroup transient-dwim nil
  "Useful preset transient commands."
  :prefix "transient-dwim-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/transient-dwim.el"))


;;; Functions

(eval
 (eval-when-compile
   `(progn
      ,@(mapcan
         (lambda (elm)
           (seq-let (pkg fns) elm
             (mapcar
              (lambda (elm*)
                `(declare-function ,elm* ,(symbol-name pkg)))
              fns)))
         '((magit
            (magit-commit-create magit-commit-amend magit-commit-extend)))))))

;;; Magit
(defun transient-dwim-magit-commit-all ()
  "Commit via magit with --all argument."
  (interactive)
  (magit-commit-create '("--all")))

(defun transient-dwim-magit-amend-all ()
  "Commit via magit with --all argument."
  (interactive)
  (magit-commit-amend '("--all")))

(defun transient-dwim-magit-extend-all ()
  "Commit via magit with --all argument."
  (interactive)
  (magit-commit-extend '("--all")))

;;; transient-dwim
(defun transient-dwim-major-mode ()
  "Invoke a mode-specific transient."
  (interactive)
  (let ((mode major-mode)
        modelist find-mode)
    (while mode
      (push mode modelist)
      (setq mode (get mode 'derived-mode-parent)))
    (setq find-mode
          (cl-find-if
           (lambda (elm)
             (fboundp (intern (format "transient-dwim-%s" elm))))
           (nreverse modelist)))
    (if find-mode
        (call-interactively (intern (format "transient-dwim-%s" find-mode)))
      (error "Function transient-dwim-{%s} is not defined"
             (string-join (mapcar 'symbol-name modelist) ", ")))))


;;; Main

(defmacro transient-dwim--define-transient-command-multi (spec)
  "Define transient command with core information from SPEC."
  `(progn
     ,@(mapcar
        (lambda (elm)
          (let ((name (pop elm))
                (info (pop elm))
                (args elm))
            (let ((packages      (plist-get :packages info))
                  (docstringspec (or (plist-get :docstring info) "")))
              `(define-transient-command ,(intern (format "transient-dwim-%s" name)) ()
                 ,docstringspec
                 ,@args))))
        spec)))

;;;###autoload (autoload 'transient-dwim-dispatch "transient-dwim" nil t)
(transient-dwim--define-transient-command-multi
 ((dispatch
   nil
   ["Transient dwim"
    [("m" "Major mode"             transient-dwim-major-mode)]
    [("M-=" "Magit"                transient-dwim-magit)]])

  (dired-mode
   (:packages (((name . "dired-filter (MELPA)")
                (url  . "https://github.com/Fuco1/dired-hacks"))
               ((name . "dired-narrow (MELPA)")
                (url  . "https://github.com/Fuco1/dired-hacks"))))
   [["Mark"
     ("m"   "Mark"                 dired-mark)
     ("h"   "Mark all"             dired-mark-subdir-files)
     ("u"   "Unmark"               dired-unmark)
     ("U"   "Unmark all"           dired-unmark-all-marks)
     ("*"   "Executables"          dired-mark-executables)
     ("d"   "Directories"          dired-mark-directories)
     ("@"   "Symlinks"             dired-mark-symlinks)
     ("e"   "Garbage"              dired-flag-garbage-files)
     ("#"   "Auto save files"      dired-flag-auto-save-files)
     ("~"   "backup files"         dired-flag-backup-files)
     ("."   "Numerical backups"    dired-clean-directory)
     ("%"   "Regexp"               dired-mark-files-regexp)
     (";"   "Change mark"          dired-change-marks)
     ("t"   "Toggle mark"          dired-toggle-marks)]
    ["Command for marked files"
     ("x"   "Do action"            dired-do-flagged-delete)
     ("A"   "Find"                 dired-do-find-regexp)
     ("C"   "Copy"                 dired-do-copy)
     ("D"   "Delete"               dired-do-delete)
     ("S"   "Symlink"              dired-do-symlink)
     ("H"   "Hardlink"             dired-do-hardlink)
     ("P"   "Print"                dired-do-print)
     ("Q"   "Replace"              dired-do-find-regexp-and-replace)
     ("B"   "Elisp bytecompile"    dired-do-byte-compile)
     ("L"   "Elisp load"           dired-do-load)
     ("X"   "Shell command"        dired-do-shell-command)
     ("Z"   "Compress"             dired-do-compress)
     ("z"   "Compress to"          dired-do-compress-to)
     ("!"   "Shell command"        dired-do-shell-command)
     ("&"   "Async shell command"  dired-do-async-shell-command)]
    ["Command"
     ("RET" "Open file"            dired-find-file)
     ("=" "  Diff"                 dired-diff)
     ("j" "  Goto file"            dired-goto-file)
     ("o" "  Open in other window" dired-find-file-other-window)
     ("4" "  Display file"         dired-display-file)
     ("v" "  View file"            dired-view-file)
     ("w" "  Copy filename"        dired-copy-filename-as-kill)
     ("W" "  Open in browser"      browse-url-of-dired-file)
     ("y" "  Show file type"       dired-show-file-type)
     ("+" "  Create directory"     dired-create-directory)
     ("<" "  Jump prev directory"  dired-prev-dirline)
     (">" "  Jump next directory"  dired-next-dirline)
     ("^" "  Move up directory"    dired-up-directory)]
    ["Display"
     ("g" "  Refresh"              revert-buffer)
     ("l" "  Redisplay"            dired-do-redisplay)
     ("k" "  Kill line"            dired-do-kill-lines)
     ("s" "  Sort"                 dired-sort-toggle-or-edit)
     ("(" "  Hide detail info"     dired-hide-details-mode)
     ("i" "  Insert subdir"        dired-maybe-insert-subdir)
     ("$" "  Hide subdir"          dired-hide-subdir)
     ("M-$" "Hide subdir all"      dired-hide-subdir)]
    ["Attribute"
     ("R"   "Name"                 dired-do-rename)
     ("G"   "Group"                dired-do-chgrp)
     ("M"   "Mode"                 dired-do-chmod)
     ("O"   "Owner"                dired-do-chown)
     ("T"   "Timestamp"            dired-do-touch)]
    ["Third party"
     ("/"   "dired-filter"         ignore)
     ("n"   "dired-narrow"         ignore)
     ("V"   "dired-git"            transient-dwim-dired-mode-git)]])

  (dired-mode-git
   (:packages (((name . "dired (builtin)"))))
   [["Worktree"
     ("c"   "Commit"               ignore)
     ("S"   "Stage"                ignore)
     ("U"   "Unstage"              ignore)
     ("z"   "Stash"                ignore)
     ("X"   "Reset"                ignore)]
    ["Branch"
     ("b"   "Branch"               ignore)
     ("t"   "Tag"                  ignore)
     ("f"   "Fetch"                ignore)
     ("F"   "Pull"                 ignore)
     ("m"   "Merge"                ignore)
     ("P"   "Push"                 ignore)
     ("!"   "Run"                  ignore)]])

  (magit
   (:packages (((name . "magit (MELPA)")
                (url  . "https://github.com/magit/magit"))))
   ["Arguments"
    ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
    ("-e" "Allow empty commit"                     "--allow-empty")
    ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
    ("-n" "Disable hooks"                          ("-n" "--no-verify"))
    ("-R" "Claim authorship and reset author date" "--reset-author")
    (magit:--author :description "Override the author")
    (7 "-D" "Override the author date" "--date=" transient-read-date)
    ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
    (5 magit:--gpg-sign)
    (magit-commit:--reuse-message)]
   [["Commit"
     ("c" "  Commit"               magit-commit)
     ("M-=" "Commit -a"            transient-dwim-magit-commit-all)
     ("e" "  Extend"               magit-commit-extend)
     ("E" "  Extend -a"            transient-dwim-magit-extend-all)
     ("a" "  Amend"                magit-commit-amend)
     ("A" "  Amend -a"             transient-dwim-magit-amend-all)
     ("w" "  Reword"               magit-commit-reword)]
    ["Edit"
     ("F"   "fixup"                magit-commit-instant-fixup)
     ("S"   "squash"               magit-commit-instant-squash)
     ("s"   "Status"               magit-status)]
    ["Magit dispatch"
     ;;("A" "Apply"                magit-cherry-pick)
     ("b"   "Branch"               magit-branch)
     ("B"   "Bisect"               magit-bisect)
     ;;("c" "Commit"               magit-commit)
     ("C"   "Clone"                magit-clone)
     ("d"   "Diff"                 magit-diff)
     ("D"   "Diff (change)"        magit-diff-refresh)
     ;;("e" "Ediff (dwim)"         magit-ediff-dwim)
     ;;("E" "Ediff"                magit-ediff)
     ("f"   "Fetch"                magit-fetch)
     ;;("F" "Pull"                 magit-pull)
     ("l"   "Log"                  magit-log)
     ("L"   "Log (change)"         magit-log-refresh)]
    [""
     ("m"   "Merge"                magit-merge)
     ("M"   "Remote"               magit-remote)
     ("o"   "Submodule"            magit-submodule)
     ("O"   "Subtree"              magit-subtree)
     ("P"   "Push"                 magit-push)
     ("r"   "Rebase"               magit-rebase)
     ("t"   "Tag"                  magit-tag)
     ("T"   "Note"                 magit-notes)]
    [""
     ("V"   "Revert"               magit-revert)
     ;;("w" "Apply patches"        magit-am)
     ("W"   "Format patches"       magit-patch)
     ("X"   "Reset"                magit-reset)
     ("y"   "Show Refs"            magit-show-refs)
     ("Y"   "Cherries"             magit-cherry)
     ("z"   "Stash"                magit-stash)
     ("!"   "Run"                  magit-run)
     ("%"   "Worktree"             magit-worktree)]])))

(provide 'transient-dwim)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; transient-dwim.el ends here
