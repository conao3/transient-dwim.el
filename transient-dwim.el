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


;;; Main

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

(define-transient-command transient-dwim-dired-mode ()
  "Invoke a major-mode spesific transient"
  [["Mark"
    ("m"   "Mark this"       dired-mark)
    ("s"   "Mark all"        dired-mark-subdir-files)
    ("*"   "Executables"     dired-mark-executables)
    ("/"   "Directories"     dired-mark-directories)
    ("@"   "Symlinks"        dired-mark-symlinks)
    ("%"   "Regexp..."       dired-mark-files-regexp)
    ("c"   "Change..."       dired-change-marks)]
   ["Unmark/Move"
    ("u" "  Unmark this"     dired-unmark)
    ("U" "  Unmark all"      dired-unmark-all-marks)
    ("DEL" "Unmark backward" dired-unmark-backward)
    ("C-n" "Next mark"       dired-next-marked-file)
    ("C-p" "Prev mark"       dired-prev-marked-file)
    ("t" "  Toggle"          dired-toggle-marks)]
   ["Git"
    ("b" "Branch"  ignore)
    ("S" "Stage"   ignore)
    ("U" "Unstage" ignore)
    ("z" "Stash"   ignore)
    ("X" "Reset"   ignore)
    ("c" "Commit"  ignore)
    ("t" "Tag"     ignore)]
   [""
    ("f" "Fetch"   ignore)
    ("F" "Pull"    ignore)
    ("m" "Merge"   ignore)
    ("P" "Push"    ignore)
    ("!" "Run"     ignore)]])

(define-transient-command transient-dwim-magit ()
  "Invoke a Magit spesific transient.
This transient is based `magit-commit' and `magit-status'.

Magit:
  Package: magit (MELPA)
  URL: https://github.com/magit/magit"
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
    ("c" "  Commit"     magit-commit-create)
    ("M-=" "Commit -a"  transient-dwim-magit-commit-all)
    ("e" "  Extend"     magit-commit-extend)
    ("E" "  Extend -a"  transient-dwim-magit-extend-all)
    ("a" "  Amend"      magit-commit-amend)
    ("A" "  Amend -a"   transient-dwim-magit-amend-all)
    ("w" "  Reword"     magit-commit-reword)]
   ["Edit"
    ("F" "fixup"      magit-commit-instant-fixup)
    ("S" "squash"     magit-commit-instant-squash)]
   ["Misc"
    ("s" "Status"     magit-status)
    ("b" "Branch"     magit-branch)
    ("C" "Clone"      magit-clone)
    ("d" "Diff"       magit-diff-working-tree)
    ("f" "Fetch"      magit-fetch)
    ("F" "Pull"       magit-pull)]
   [""
    ("m" "Merge"      magit-merge)
    ("r" "Rebase"     magit-rebase)
    ("l" "Log"        magit-log)
    ("X" "Reset"      magit-reset)
    ("z" "Stash"      magit-stash)]])

;;;###autoload (autoload 'transient-dwim-dispatch "transient-dwim" nil t)
(define-transient-command transient-dwim-dispatch ()
  "Invoke a transient-dwim command."
  ["Transient dwim"
   [("m" "Major mode" transient-dwim-major-mode)]
   [("M-=" "Magit"    transient-dwim-magit)]])

(provide 'transient-dwim)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; transient-dwim.el ends here
