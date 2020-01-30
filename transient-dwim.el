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

(defmacro transient-dwim--declare-function (spec)
  "Declare function for SPEC."
  `(progn
     ,@(mapcan
        (lambda (elm)
          (seq-let (pkg fns) elm
            (mapcar
             (lambda (elm*)
               `(declare-function ,elm* ,(symbol-name pkg)))
             fns)))
        spec)))

(transient-dwim--declare-function
 ((magit
   (magit-commit-create magit-commit-amend magit-commit-extend))))

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


;;; Main

(eval-and-compile
  (defvar transient-dwim-docstring-format
    "Transient-dwim for `%p'.

Depends packages:
%P
URL:
%U
This transient command is defined by `transient-dwim--define-transient-command-multi'.
If you want to customize this transient command, please use transient
customize scheme, https://magit.vc/manual/transient/Modifying-Existing-Transients.html

Or, please send a Issue/PR to https://github.com/conao3/transient-dwim.el"

    "The format string used to docstring for transients function.

The following %-sequences are supported:
  `%p': Target package name.
  `%p': Depends packages name.
  `%U': Depends packages URL.")

  (defun transient-dwim--create-docstring (pkg info)
    "Create docstring from PKG and transient-command-multi INFO."
    (let ((dep-pkgs-info (plist-get info :packages))
          (docstringspec (or (plist-get :docstring info) transient-dwim-docstring-format)))
      (let ((dep-pkgs-name (mapcar (lambda (elm) (alist-get 'name elm)) dep-pkgs-info))
            (dep-pkgs-url  (mapcar (lambda (elm) (alist-get 'url elm)) dep-pkgs-info)))
        (format-spec
         docstringspec
         `((?p . ,pkg)
           (?P . ,(if dep-pkgs-info
                      (cl-loop for elm in dep-pkgs-name
                               for i from 1
                               when elm
                               concat (format "  - %s [%d]\n" elm i))
                    "  None\n"))
           (?U . ,(if dep-pkgs-info
                      (cl-loop for elm in dep-pkgs-url
                               for i from 1
                               when elm
                               concat (format "  - %s [%d]\n" elm i))
                    "  None\n"))))))))

(defmacro transient-dwim--define-transient-command-multi (spec)
  "Define transient command with core information from SPEC."
  `(prog1 'transient-dwim
     ,@(mapcar
        (lambda (elm)
          (let ((pkg  (pop elm))
                (info (pop elm))
                (args elm))
            `(define-transient-command ,(intern (format "transient-dwim-%s" pkg)) ()
               ,(transient-dwim--create-docstring pkg info)
               ,@args)))
        spec)))

;;;###autoload (autoload 'transient-dwim-dispatch "transient-dwim" nil t)
(transient-dwim--define-transient-command-multi
 ((dispatch
   nil
   ["Dired-mode"
    :if-derived dired-mode
    ["Mark"
     ("mm"  "Mark"                 dired-mark)
     ("mM"  "Mark all"             dired-mark-subdir-files)
     ("mu"  "Unmark"               dired-unmark)
     ("mU"  "Unmark all"           dired-unmark-all-marks)
     ("m*"  "Executables"          dired-mark-executables)
     ("m/"  "Directories"          dired-mark-directories)
     ("m@"  "Symlinks"             dired-mark-symlinks)
     ("m&"  "Garbage files"        dired-flag-garbage-files)
     ("m#"  "Auto save files"      dired-flag-auto-save-files)
     ("m~"  "backup files"         dired-flag-backup-files)
     ("m."  "Numerical backups"    dired-clean-directory)
     ("m%"  "Regexp"               dired-mark-files-regexp)
     ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)
     ("mc"  "Change mark"          dired-change-marks)
     ("mt"  "Toggle mark"          dired-toggle-marks)]
    ["Command for marked files"
     ("x"   "Do action"            dired-do-flagged-delete)
     ("C"   "Copy"                 dired-do-copy)
     ("D"   "Delete"               dired-do-delete)
     ("S"   "Symlink"              dired-do-symlink)
     ("H"   "Hardlink"             dired-do-hardlink)
     ("P"   "Print"                dired-do-print)
     ("A"   "Find"                 dired-do-find-regexp)
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
     ("o" "  Open in other window" dired-find-file-other-window)
     ("C-o" "Open in other window (No select)" dired-display-file)
     ("v" "  Open file (View mode)"dired-view-file)
     ("=" "  Diff"                 dired-diff)
     ("j" "  Goto file"            dired-goto-file)
     ("w" "  Copy filename"        dired-copy-filename-as-kill)
     ("W" "  Open in browser"      browse-url-of-dired-file)
     ("y" "  Show file type"       dired-show-file-type)
     ("+" "  Create directory"     dired-create-directory)
     ("<" "  Jump prev directory"  dired-prev-dirline)
     (">" "  Jump next directory"  dired-next-dirline)
     ("^" "  Move up directory"    dired-up-directory)]
    ["Display"
     ("g" "  Refresh buffer"       revert-buffer)
     ("l" "  Refresh file"         dired-do-redisplay)
     ("k" "  Remove line"          dired-do-kill-lines)
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
    ["Extension"
     ("e"   "wdired"               wdired-change-to-wdired-mode)
     ("p"   "image-dired"          transient-dwim-dired-mode--image)
     (":"   "epa-dired"            transient-dwim-dired-mode--epa)
     ("/"   "dired-filter"         ignore)
     ("n"   "dired-narrow"         ignore)
     ("V"   "dired-git"            transient-dwim-dired-mode--git)]]

   ["Image-dired-thumbnail-mode"
    :if-derived image-dired-thumbnail-mode
    ["Commands"
     ("d" "  Mark as delete"       image-dired-flag-thumb-original-file)
     ("m" "  Mark"                 image-dired-mark-thumb-original-file)
     ("u" "  Unmark"               image-dired-unmark-thumb-original-file)
     ("." "  Track"                image-dired-track-original-file)
     ("TAB" "Jump dired"           image-dired-jump-original-dired-buffer)
     ("gf" " Line up"              image-dired-line-up)
     ("gg" " Line up (dynamic)"    image-dired-line-up-dynamic)
     ("gi" " Line up (interactive)"image-dired-line-up-interactive)
     ("tt" " Tag"                  image-dired-tag-thumbnail)
     ("tr" " Delete tag"           image-dired-tag-thumbnail-remove)

     ("RET" "Open image"           image-dired-display-thumbnail-original-image)
     ("E" "  Open image external"  image-dired-thumbnail-display-external)

     ("l" "  Rotate left"          image-dired-rotate-thumbnail-left)
     ("r" "  Rotate right"         image-dired-rotate-thumbnail-right)
     ("L" "  Original rotate left" image-dired-rotate-original-left)
     ("R" "  Original rotate right"image-dired-rotate-original-right)

     ("D" "  Add description"      image-dired-thumbnail-set-image-description)
     ("C-d" "Delete thumnail"      image-dired-delete-char)
     ("SPC" "Show image and next"  image-dired-display-next-thumbnail-original)
     ("DEL" "Show image and prev"  image-dired-display-previous-thumbnail-original)
     ("c" "  Add comment"          image-dired-comment-thumbnail)]]

   ["Extension"
    [("M-=" "Magit"                transient-dwim-magit :if (lambda () (require 'magit nil t)))]])

  (dired-mode--image
   (:packages (((name . "image-dired (builtin)"))))
   ["Commands"
    ("d"    "Open thumbnail buffer"image-dired-display-thumbs)
    ("j"    "Jump thumbnail buffer"image-dired-jump-thumbnail-buffer)
    ("a"    "Append thumnail buffer" image-dired-display-thumbs-append)
    ("i"    "Inline thumnail"      image-dired-dired-toggle-marked-thumbs)
    ("f"    "Display image"        image-dired-dired-display-image)
    ("x"    "Open external"        image-dired-dired-display-external)
    ("m"    "Mark via tag"         image-dired-mark-tagged-files)
    ("t"    "Edit tag"             image-dired-tag-files)
    ("r"    "Delete tag"           image-dired-delete-tag)
    ("c"    "Edit comment"         image-dired-dired-comment-files)
    ("e"    "Edit comment/tag"     image-dired-dired-edit-comment-and-tags)])

  (dired-mode--git
   (:packages (((name . "dired (builtin)"))))
   [["Worktree"
     ("c"   "Commit"               dired-git-commit)
     ("S"   "Stage"                dired-git-stage)
     ("U"   "Unstage"              dired-git-unstage)
     ("zz"  "Stash"                dired-git-stash)
     ("zp"  "Stash pop"            dired-git-stash-pop)
     ("X"   "Reset --hard"         dired-git-reset-hard)]
    ["Branch"
     ("b"   "Branch"               dired-git-branch)
     ("t"   "Tag"                  dired-git-tag)
     ("f"   "Fetch"                dired-git-fetch)
     ("F"   "Pull"                 dired-git-pull)
     ("m"   "Merge"                dired-git-merge)
     ("P"   "Push"                 dired-git-push)
     ("!"   "Run"                  dired-git-run)]])

  (dired-mode--epa
   (:packages (((name . "epa-dired (builtin)"))))
   ["Commands"
    ("e"    "Encrypt"              epa-dired-do-encrypt)
    ("d"    "Decrypt"              epa-dired-do-decrypt)
    ("v"    "Verify"               epa-dired-do-verify)
    ("s"    "Sign"                 epa-dired-do-sign)])

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
     ("U"   "fixup"                magit-commit-instant-fixup)
     ("S"   "squash"               magit-commit-instant-squash)
     ("s"   "Status"               magit-status)]
    ["Magit dispatch"
     ;; ("A" "Apply"               magit-cherry-pick)
     ("b"   "Branch"               magit-branch)
     ("B"   "Bisect"               magit-bisect)
     ;; ("c" "Commit"              magit-commit)
     ("C"   "Clone"                magit-clone)
     ("d"   "Diff"                 magit-diff)
     ("D"   "Diff (change)"        magit-diff-refresh)
     ;; ("e" "Ediff (dwim)"        magit-ediff-dwim)
     ;; ("E" "Ediff"               magit-ediff)
     ("f"   "Fetch"                magit-fetch)
     ("F"   "Pull"                 magit-pull)
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
     ;; ("w" "Apply patches"       magit-am)
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
