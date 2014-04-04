;;; dired-k.el --- highlighting dired line like k.sh

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-dired-k
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup dired-k nil
  "k.sh in dired"
  :group 'dired)

(defface dired-k-modified
  '((t (:foreground "red" :weight bold)))
  "Face of modified file in git repository"
  :group 'dired-k)

(defface dired-k-commited
  '((t (:foreground "green" :weight bold)))
  "Face of commited file in git repository"
  :group 'dired-k)

(defface dired-k-added
  '((t (:foreground "magenta" :weight bold)))
  "Face of added file in git repository"
  :group 'dired-k)

(defface dired-k-untracked
  '((t (:foreground "orange" :weight bold)))
  "Face of untracked file in git repository"
  :group 'dired-k)

(defface dired-k-directory
  '((t (:foreground "blue")))
  "Face of directory"
  :group 'dired-k)

(defcustom dired-k-size-colors
  '((1024 . "chartreuse4") (2048 . "chartreuse3") (3072 . "chartreuse2")
    (5120 . "chartreuse1") (10240 . "yellow3") (20480 . "yellow2") (40960 . "yellow")
    (102400 . "orange3") (262144 . "orange2") (524288 . "orange"))
  "assoc of file size and color"
  :group 'dired-k)

(defcustom dired-k-date-colors
  '((0 . "red") (60 . "white") (3600 . "grey80")
    (86400 . "grey70") (604800 . "grey40") (2419200 . "grey40")
    (15724800 . "grey30") (31449600 . "grey25") (62899200 . "grey10"))
  "assoc of file modified time and color"
  :group 'dired-k)

(defsubst dired-k--git-status-color (stat)
  (cl-case stat
    (modified 'dired-k-modified)
    (normal 'dired-k-commited)
    (added 'dired-k-added)
    (untracked 'dired-k-untracked)
    (ignored 'dired-k-ignored)))

(defsubst dired-k--decide-status (status)
  (cond ((string= status " M") 'modified)
        ((string= status "??") 'untracked)
        ((string= status "!!") 'ignored)
        ((string= status "A ") 'added)
        (t 'normal)))

(defsubst dired-k--subdir-status (current-status new-status)
  (cond ((eq current-status 'modified) 'modified)
        ((eq new-status 'added) 'added)
        (t 'normal)))

(defun dired-k--parse-git-status ()
  (goto-char (point-min))
  (let ((files-status (make-hash-table :test 'equal)))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (status (dired-k--decide-status (substring line 0 2)))
             (file (substring line 3)))
        (if (string-match "\\`\\(.*+?\\)/" file)
            (let* ((subdir (match-string 1 file))
                   (cur-status (gethash subdir files-status)))
              (puthash subdir (dired-k--subdir-status cur-status status)
                       files-status))
          (puthash file status files-status)))
      (forward-line 1))
    files-status))

(defun dired-k--collect-git-status ()
  (let ((cmd "git status --porcelain --ignored --untracked-files=normal"))
    (with-temp-buffer
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: %s" cmd))
      (dired-k--parse-git-status))))

(defsubst dired-k--root-directory ()
  (expand-file-name (locate-dominating-file default-directory ".git/")))

(defun dired-k--highlight-line (root file stats)
  (let* ((relpath (file-relative-name file root))
         (stat (gethash relpath stats 'normal)))
    (unless (and (file-directory-p filename) (eq stat 'normal))
      (let ((ov (make-overlay (1- (point)) (point)))
            (stat-face (dired-k--git-status-color stat)))
        (overlay-put ov 'display (propertize "|" 'face stat-face))))))

(defun dired-k--highlight-git-information ()
  (let ((stats (dired-k--collect-git-status))
        (root (dired-k--root-directory)))
    (save-excursion
      (goto-char (point-min))
      (dired-next-line 2)
      (while (not (eobp))
        (let ((filename (dired-get-filename nil t)))
          (when filename
            (dired-k--highlight-line root filename stats)))
        (dired-next-line 1)))))

(defsubst dired-k--size-face (size)
  (cl-loop for (border . color) in dired-k-size-colors
           when (< size border)
           return `((:foreground ,color :weight bold))
           finally
           return '((:foreground "red" :weight bold))))

(defsubst dired-k--date-face (modified-time)
  (cl-loop with current-time = (float-time (current-time))
           with diff = (- current-time modified-time)
           for (val . color) in dired-k-date-colors
           when (< diff val)
           return `((:foreground ,color :weight bold))
           finally
           return '((:foreground "grey50" :weight bold))))

(defun dired-k--highlight-by-size (size start end)
  (let ((ov (make-overlay start end))
        (size-face (dired-k--size-face size)))
    (overlay-put ov 'face size-face)))

(defun dired-k--highlight-by-date (modified-time start end)
  (let* ((ov (make-overlay start end))
         (size-face (dired-k--date-face (float-time modified-time))))
    (overlay-put ov 'face size-face)))

(defsubst dired-k--size-to-regexp (size)
  (concat "\\_<" (number-to-string size) "\\_>"))

(defun dired-k--highlight-directory ()
  (save-excursion
    (back-to-indentation)
    (when (eq (char-after) ?d)
      (let ((ov (make-overlay (point) (1+ (point)))))
        (overlay-put ov 'face 'dired-k-directory)))))

(defun dired-k--highlight-by-file-attribyte ()
  (save-excursion
    (goto-char (point-min))
    (dired-next-line 2)
    (while (not (eobp))
      (let* ((file-attrs (file-attributes (dired-get-filename nil t)))
             (modified-time (nth 5 file-attrs))
             (file-size (nth 7 file-attrs))
             (date-end-point (1- (point))))
        (dired-k--highlight-directory)
        (when (and file-size
                   (re-search-backward (dired-k--size-to-regexp file-size) nil t))
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (dired-k--highlight-by-size file-size start end)
            (goto-char end)
            (skip-chars-forward "^ \t")
            (skip-chars-forward " \t")
            (dired-k--highlight-by-date modified-time (point) date-end-point)))
        (dired-next-line 1)))))

(defun dired-k--inside-git-repository-p ()
  (when (executable-find "git")
    (with-temp-buffer
      (let ((cmd "git rev-parse --is-inside-work-tree"))
        (when (zerop (call-process-shell-command cmd nil t))
          (goto-char (point-min))
          (string= "true" (buffer-substring-no-properties
                           (point) (line-end-position))))))))

;;;###autoload
(defun dired-k ()
  (interactive)
  (save-excursion
    (dired-k--highlight-by-file-attribyte)
    (when (dired-k--inside-git-repository-p)
      (dired-k--highlight-git-information))))

(provide 'dired-k)

;;; dired-k.el ends here
