;;; transient-dwim.el --- Useful preset transient commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Transient-Dwim-Requires: ((emacs "25.1") (transient "0.1.0"))
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


;;; Code:

(require 'transient)

(defgroup transient-dwim nil
  "Useful preset transient commands."
  :prefix "transient-dwim-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/transient-dwim.el"))


;;; Main

(define-transient-command transient-dwim-major-mode ()
  "Invoke a major-mode spesific transient"
  ["Dired-mode"
   :if-derived dired-mode
   ["Mark"
    [("m"   "Mark this"       dired-mark)
     ("s"   "Mark all"        dired-mark-subdir-files)
     ("*"   "Executables"     dired-mark-executables)
     ("/"   "Directories"     dired-mark-directories)
     ("@"   "Symlinks"        dired-mark-symlinks)
     ("%"   "Regexp..."       dired-mark-files-regexp)
     ("c"   "Change..."       dired-change-marks)]
    [("u"   "Unmark this"     dired-unmark)
     ("U"   "Unmark all"      dired-unmark-all-marks)]
    [("DEL" "Unmark backward" dired-unmark-backward)
     ("C-n" "Next mark"       dired-next-marked-file)
     ("C-p" "Prev mark"       dired-prev-marked-file)
     ("t"   "Toggle"          dired-toggle-marks)]]])

;;;###autoload (autoload 'transient-dwim-dispatch "transient-dwim" nil t)
(define-transient-command transient-dwim-dispatch ()
  "Invoke a transient-dwim command."
  ["Transient dwim"
   ("m" "Major mode" transient-dwim-major-mode)])

(provide 'transient-dwim)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; transient-dwim.el ends here
