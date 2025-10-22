;;; mini-essays.el --- Focused mini-essay writing with enforced limits -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daljeet singh

;; Author: Daljeet singh
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: writing, org, productivity
;; URL: https://github.com/D4lj337/Mini-essays
;; License: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Mini-essays mode provides a focused writing environment for short-form
;; essays in Org-mode with enforced character/word limits, visual feedback,
;; and automatic templates.
;;
;; Features:
;; - Hard character/word limits with paste protection
;; - Real-time character counter in mode line
;; - Multiple limit profiles (micro, mini, short, medium)
;; - Visual warning zones (color-coded feedback)
;; - Export with statistics and metadata
;; - Auto-insert templates for new files
;;
;; Usage:
;;   Enable globally for mini*.org files:
;;     (add-hook 'org-mode-hook #'mini-essays-setup)
;;
;;   Or use interactively:
;;     M-x mini-essays-mode

;;; Code:

(require 'org)
(require 'autoinsert)

;;; Customization

(defgroup mini-essays nil
  "Focused mini-essay writing with enforced limits."
  :group 'org
  :prefix "mini-essays-")

(defcustom mini-essays-max-chars 4000
  "Maximum allowed characters for mini essays."
  :type 'integer
  :group 'mini-essays)

(defcustom mini-essays-max-words 500
  "Maximum allowed words when using word-count mode."
  :type 'integer
  :group 'mini-essays)

(defcustom mini-essays-limit-type 'characters
  "Type of limit enforcement: 'characters or 'words."
  :type '(choice (const :tag "Characters" characters)
                 (const :tag "Words" words))
  :group 'mini-essays)

(defcustom mini-essays-fill-column 72
  "Fill column for mini essay buffers."
  :type 'integer
  :group 'mini-essays)

(defcustom mini-essays-profiles
  '((micro . 500)
    (mini . 2000)
    (short . 4000)
    (medium . 8000))
  "Predefined character limit profiles."
  :type '(alist :key-type symbol :value-type integer)
  :group 'mini-essays)

(defcustom mini-essays-warning-threshold 80
  "Percentage at which to show warning color (yellow)."
  :type 'integer
  :group 'mini-essays)

(defcustom mini-essays-critical-threshold 95
  "Percentage at which to show critical color (red)."
  :type 'integer
  :group 'mini-essays)

(defcustom mini-essays-file-pattern "\\(?:/\\|\\`\\)mini[^/]*\\.org\\'"
  "Regex pattern matching mini essay files."
  :type 'string
  :group 'mini-essays)

;;; Internal Variables

(defvar-local mini-essays--last-count 0
  "Cache for last character/word count.")

;;; Utility Functions

(defun mini-essays--matching-file-p ()
  "Check if current buffer is a mini essay file."
  (and (derived-mode-p 'org-mode)
       (buffer-file-name)
       (string-match-p mini-essays-file-pattern (buffer-file-name))))

(defun mini-essays--over-limit-p ()
  "Check if current buffer exceeds the configured limit."
  (if (eq mini-essays-limit-type 'words)
      (> (count-words (point-min) (point-max)) mini-essays-max-words)
    (> (buffer-size) mini-essays-max-chars)))

(defun mini-essays--current-count ()
  "Return current count (chars or words) based on limit type."
  (if (eq mini-essays-limit-type 'words)
      (count-words (point-min) (point-max))
    (buffer-size)))

(defun mini-essays--max-count ()
  "Return maximum count based on limit type."
  (if (eq mini-essays-limit-type 'words)
      mini-essays-max-words
    mini-essays-max-chars))

(defun mini-essays--limit-type-string ()
  "Return human-readable limit type string."
  (if (eq mini-essays-limit-type 'words) "words" "chars"))

;;; Core Enforcement Functions

(defun mini-essays--enforce-limit ()
  "Prevent inserting beyond configured limit."
  (when (and (mini-essays--matching-file-p)
             (mini-essays--over-limit-p))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (undo-boundary)
      (backward-delete-char-untabify 1)
      (message "Limit %d %s reached"
               (mini-essays--max-count)
               (mini-essays--limit-type-string)))))

(defun mini-essays--yank-guard (orig-fun &rest args)
  "Prevent yanking that would exceed the limit."
  (if (mini-essays--matching-file-p)
      (let ((before (mini-essays--current-count)))
        (apply orig-fun args)
        (when (mini-essays--over-limit-p)
          (let* ((current (mini-essays--current-count))
                 (max-allowed (mini-essays--max-count))
                 (excess (- current max-allowed)))
            (delete-region (- (point) excess) (point))
            (message "Trimmed yank to %d %s"
                     max-allowed
                     (mini-essays--limit-type-string)))))
    (apply orig-fun args)))

;;; Visual Feedback Functions

(defun mini-essays--update-modeline ()
  "Update mode line with character count and visual feedback."
  (when (and (bound-and-true-p mini-essays-mode)
             (mini-essays--matching-file-p))
    (let* ((current (mini-essays--current-count))
           (max-count (mini-essays--max-count))
           (remaining (- max-count current))
           (percent (if (> max-count 0)
                        (/ (* 100.0 current) max-count)
                      0))
           (type-str (mini-essays--limit-type-string)))

      ;; Update modeline with count
      (setq mini-essays--last-count current)

      ;; Update visual feedback based on percentage
      (cond
       ((>= percent mini-essays-critical-threshold)
        (setq-local mode-line-buffer-identification
                    (list (propertize (format "%%b [%d/%d %s]" current max-count type-str)
                                      'face '(:foreground "red" :weight bold)))))
       ((>= percent mini-essays-warning-threshold)
        (setq-local mode-line-buffer-identification
                    (list (propertize (format "%%b [%d/%d %s]" current max-count type-str)
                                      'face '(:foreground "orange" :weight bold)))))
       (t
        (setq-local mode-line-buffer-identification
                    (list (propertize (format "%%b [%d/%d %s]" current max-count type-str)
                                      'face 'mode-line-buffer-id)))))

      (force-mode-line-update))))

;;; Profile Management

;;;###autoload
(defun mini-essays-set-profile (profile)
  "Set character limit based on predefined PROFILE."
  (interactive
   (list (intern (completing-read "Select profile: "
                                  (mapcar #'car mini-essays-profiles)
                                  nil t))))
  (let ((limit (alist-get profile mini-essays-profiles)))
    (if limit
        (progn
          (setq-local mini-essays-max-chars limit)
          (mini-essays--update-modeline)
          (message "Character limit set to %d (%s profile)" limit profile))
      (user-error "Unknown profile: %s" profile))))

;;;###autoload
(defun mini-essays-toggle-limit-type ()
  "Toggle between character and word limit modes."
  (interactive)
  (setq-local mini-essays-limit-type
              (if (eq mini-essays-limit-type 'characters)
                  'words
                'characters))
  (mini-essays--update-modeline)
  (message "Limit type: %s (max: %d)"
           (mini-essays--limit-type-string)
           (mini-essays--max-count)))

;;; Export Functions

;;;###autoload
(defun mini-essays-export-with-stats ()
  "Export mini essay with embedded statistics."
  (interactive)
  (save-excursion
    (let* ((chars (buffer-size))
           (words (count-words (point-min) (point-max)))
           (reading-time (ceiling (/ words 200.0)))
           (stats-string (format "\n\n---\n*Statistics:* %d words, %d chars, ~%d min read\n*Completed:* %s\n"
                                 words chars reading-time
                                 (format-time-string "%Y-%m-%d %H:%M"))))
      (goto-char (point-max))
      (unless (search-backward "---" nil t)
        (goto-char (point-max))
        (insert stats-string)
        (message "Statistics added to buffer")))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode mini-essays-mode
  "Minor mode for focused mini-essay writing with enforced limits."
  :init-value nil
  :lighter " Mini"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m p") #'mini-essays-set-profile)
            (define-key map (kbd "C-c m t") #'mini-essays-toggle-limit-type)
            (define-key map (kbd "C-c m e") #'mini-essays-export-with-stats)
            map)
  (if mini-essays-mode
      (progn
        ;; Enable enforcement
        (add-hook 'post-self-insert-hook #'mini-essays--enforce-limit nil t)
        (add-hook 'after-change-functions
                  (lambda (&rest _) (mini-essays--update-modeline)) nil t)
        (advice-add 'yank :around #'mini-essays--yank-guard)
        (advice-add 'yank-pop :around #'mini-essays--yank-guard)

        ;; Configure buffer
        (setq-local fill-column mini-essays-fill-column)
        (auto-fill-mode 1)
        (display-fill-column-indicator-mode 1)

        ;; Initial modeline update
        (mini-essays--update-modeline)

        (message "Mini-essays mode enabled"))

    ;; Disable mode
    (remove-hook 'post-self-insert-hook #'mini-essays--enforce-limit t)
    (remove-hook 'after-change-functions
                 (lambda (&rest _) (mini-essays--update-modeline)) t)
    (advice-remove 'yank #'mini-essays--yank-guard)
    (advice-remove 'yank-pop #'mini-essays--yank-guard)
    (setq-local mode-line-buffer-identification '("%b"))
    (force-mode-line-update)
    (message "Mini-essays mode disabled")))

;;; Auto-setup Function

;;;###autoload
(defun mini-essays-setup ()
  "Automatically setup mini-essays mode for matching files."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (string-match-p mini-essays-file-pattern (buffer-file-name)))
    (mini-essays-mode 1)))

;;; Auto-insert Template

;;;###autoload
(define-skeleton mini-essays-org-skeleton
  "Header/template for new mini*.org files."
  nil
  "#+title: " (file-name-base (or (buffer-file-name) (buffer-name))) "\n"
  "#+author: " (or user-full-name "") "\n"
  "#+date: " (format-time-string "%Y-%m-%d") "\n"
  "#+options: toc:nil num:nil\n"
  "#+property: reference: " (skeleton-read "Reference (URL, ID, or note): ") "\n"
  "#+property: limit: " (number-to-string mini-essays-max-chars) "\n"
  "\n"
  "* Draft\n"
  "\n")

;;;###autoload
(defun mini-essays-setup-auto-insert ()
  "Configure auto-insert for mini essay files."
  (interactive)
  (unless (boundp 'auto-insert-mode)
    (require 'autoinsert))
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (add-to-list 'auto-insert-alist
               `((,mini-essays-file-pattern . "Mini Essay")
                 . mini-essays-org-skeleton)))

;; Auto-setup auto-insert when package loads
(with-eval-after-load 'autoinsert
  (mini-essays-setup-auto-insert))

;;; Footer

(provide 'mini-essays)

;;; mini-essays.el ends here
