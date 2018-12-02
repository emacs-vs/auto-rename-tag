;;; auto-rename-tag.el --- Automatically rename paired HTML/XML tag.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-12-01 23:56:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically rename paired HTML/XML tag.
;; Keyword: auto-complete html rename tag xml
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/auto-rename-tag

;; This file is NOT part of GNU Emacs.

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
;;
;; Automatically rename paired HTML/XML tag.
;;

;;; Code:

(defgroup auto-rename-tag nil
  "Automatically rename paired HTML/XML tag."
  :prefix "auto-rename-tag-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/auto-rename-tag.git"))


(defvar auto-rename-tag-pre-command-actived nil
  "Check if `pre-command-hook' called.")

(defvar auto-rename-tag-record-prev-word ""
  "Record down the word in `pre-command-hook'.")


(defun auto-rename-tag-delete-word (arg)
  "Delete the current words (ARG) and do not push it to kill ring."
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun auto-rename-tag-is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (= (point) (point-min)))

(defun auto-rename-tag-is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (= (point) (point-max)))

(defun auto-rename-tag-current-char-equal-p (c)
  "Check the current character equal to 'C'."
  (if (or (auto-rename-tag-is-beginning-of-buffer-p)
          (auto-rename-tag-is-end-of-buffer-p))
      nil
    (let ((current-char-string (string (char-before))))
      (string= current-char-string c))))

(defun auto-rename-tag-backward-goto-char (c)
  "Goto backward character match 'C'."
  (while (and (not (auto-rename-tag-is-beginning-of-buffer-p))
              (not (auto-rename-tag-current-char-equal-p c)))
    (backward-char 1)))

(defun auto-rename-tag-forward-goto-char (c)
  "Goto forward character match 'C'."
  (when (not (auto-rename-tag-is-end-of-buffer-p))
    (forward-char 1))
  (while (and (not (auto-rename-tag-is-end-of-buffer-p))
              (not (auto-rename-tag-current-char-equal-p c)))
    (forward-char 1)))

(defun auto-rename-tag-backward-char-at-point (c)
  "Search backward for 'C' and return the point.
If not found, return -1."
  (save-excursion
    (auto-rename-tag-backward-goto-char c)
    (if (auto-rename-tag-current-char-equal-p c)
        (point)
      -1)))

(defun auto-rename-tag-forward-char-at-point (c)
  "Search forward for 'C' and return the point.
If not found, return -1."
  (save-excursion
    (auto-rename-tag-forward-goto-char c)
    (if (auto-rename-tag-current-char-equal-p c)
        (point)
      -1)))

(defun auto-rename-tag-get-current-char-string ()
  "Get the current character as the 'string'."
  (string (char-before)))

(defun auto-rename-tag-inside-tag ()
  "Check if current point inside the tag."
  (let ((backward-less (auto-rename-tag-backward-char-at-point "<"))
        (backward-greater (auto-rename-tag-backward-char-at-point ">"))
        (forward-less (auto-rename-tag-forward-char-at-point "<"))
        (forward-greater (auto-rename-tag-forward-char-at-point ">")))
    (and (not (= -1 backward-less))
         (not (= -1 forward-greater))
         (< backward-greater backward-less)
         (or (< forward-greater forward-less)
             (= -1 forward-less)))))

(defun auto-rename-tag-goto-backward-tag ()
  "Goto the backward tag."
  (when (auto-rename-tag-inside-tag)
    (auto-rename-tag-backward-goto-char "<")
    (backward-char 1))
  (while (and (not (auto-rename-tag-is-beginning-of-buffer-p))
              (not (auto-rename-tag-inside-tag)))
    (auto-rename-tag-backward-goto-char "<")))

(defun auto-rename-tag-goto-forward-tag ()
  "Goto the forward tag."
  (when (auto-rename-tag-inside-tag)
    (auto-rename-tag-forward-goto-char "<"))
  (while (and (not (auto-rename-tag-is-end-of-buffer-p))
              (not (auto-rename-tag-inside-tag)))
    (auto-rename-tag-forward-goto-char "<")))

(defun auto-rename-tag-backward-count-nested-close-tag (&optional nc)
  "Search backward, returns the count of the nested closing tag.
NC : recursive nested count."
  (save-excursion
    (let ((nested-count 0))
      (when nc
        (setq nested-count nc))

      (auto-rename-tag-goto-backward-tag)
      (forward-char 1)

      ;; If closing tag.
      (when (auto-rename-tag-current-char-equal-p "/")
        (setq nested-count (+ nested-count 1))
        (setq nested-count (auto-rename-tag-backward-count-nested-close-tag nested-count)))
      nested-count)))

(defun auto-rename-tag-forward-count-nested-open-tag (&optional nc)
  "Search forward, returns the count of the nested opening tag.
NC : recursive nested count."
  (save-excursion
    (let ((nested-count 0))
      (when nc
        (setq nested-count nc))

      (auto-rename-tag-goto-forward-tag)
      (forward-char 1)

      ;; If closing tag.
      (when (not (auto-rename-tag-current-char-equal-p "/"))
        (setq nested-count (+ nested-count 1))
        (setq nested-count (auto-rename-tag-forward-count-nested-open-tag nested-count)))
      nested-count)))

(defun okay-test ()
  (interactive)
  (message "some : %s" (auto-rename-tag-forward-count-nested-open-tag))
  )

(defun auto-rename-tag-before-change-functions (begin end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."

  ;; Reset record.
  (setq auto-rename-tag-record-prev-word "")
  ;; Reset flag.
  (setq auto-rename-tag-pre-command-actived nil)

  (when (auto-rename-tag-inside-tag)
    (save-excursion
      ;; Set active flag.
      (setq auto-rename-tag-pre-command-actived t)

      ;; Record it down.
      (if (auto-rename-tag-current-char-equal-p "<")
          (setq auto-rename-tag-record-prev-word (thing-at-point 'word))
        (progn
          (auto-rename-tag-backward-goto-char "<")

          (while (and (not (auto-rename-tag-is-end-of-buffer-p))
                      ;; Is the word, nil?
                      (not (thing-at-point 'word)))
            (forward-char 1))

          ;; Get the first word in the tag.
          (setq auto-rename-tag-record-prev-word (thing-at-point 'word)))))))

(defun auto-rename-tag-after-change-function (begin end length)
  "Do stuff after buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes.
LENGTH : deletion length."
  (when (and auto-rename-tag-pre-command-actived
             (auto-rename-tag-inside-tag))
    (save-excursion
      (let ((first-char-point-in-tag -1)
            (end-tag nil)
            (current-word "")
            (pair-tag-word ""))

        ;; Goto the first character inside the tag.
        (setq first-char-point-in-tag (+ (auto-rename-tag-backward-char-at-point "<") 1))
        (goto-char first-char-point-in-tag)

        (cond ((auto-rename-tag-current-char-equal-p "/")
               ;; If the first char is '/', meaning is the end tag.
               (setq end-tag t))
              ((auto-rename-tag-current-char-equal-p ">")
               ;; If this is true, meaning the tag is empty.
               (backward-char 1)))

        ;; Get the current word, if not nil.
        (when (thing-at-point 'word)
          (setq current-word (thing-at-point 'word)))

        (when (not (string= current-word auto-rename-tag-record-prev-word))
          (if end-tag
              (progn
                (while (and (not (auto-rename-tag-is-beginning-of-buffer-p))
                            (not (string= auto-rename-tag-record-prev-word pair-tag-word)))
                  (auto-rename-tag-goto-backward-tag)
                  (setq pair-tag-word (thing-at-point 'word)))

                (when (string= auto-rename-tag-record-prev-word pair-tag-word)
                  (forward-char 1)

                  ;; Delete the pair word.
                  (auto-rename-tag-delete-word 1)

                  ;; Insert new word.
                  (insert current-word)))
            (progn
              ;; First, move out of tag.
              (auto-rename-tag-forward-goto-char ">")

              (while (and (not (auto-rename-tag-is-end-of-buffer-p))
                          (not (string= auto-rename-tag-record-prev-word pair-tag-word)))
                (auto-rename-tag-goto-forward-tag)
                (when (not (auto-rename-tag-is-end-of-buffer-p))
                  ;; Escape slash.
                  (forward-char 1))
                (setq pair-tag-word (thing-at-point 'word)))

              (when (string= auto-rename-tag-record-prev-word pair-tag-word)
                ;; Delete the pair word.
                (auto-rename-tag-delete-word 1)

                ;; Insert new word.
                (insert current-word)))))))))


(defun auto-rename-tag-enable ()
  "Enable `auto-rename-tag' in current buffer."
  (add-hook 'before-change-functions #'auto-rename-tag-before-change-functions nil t)
  (add-hook 'after-change-functions #'auto-rename-tag-after-change-function nil t))

(defun auto-rename-tag-disable ()
  "Disable `auto-rename-tag' in current buffer."
  (remove-hook 'before-change-functions #'auto-rename-tag-before-change-functions t)
  (remove-hook 'after-change-functions #'auto-rename-tag-after-change-function t))


(define-minor-mode auto-rename-tag-mode
  "Minor mode 'auto-rename-tag' mode."
  :lighter " ART"
  :group auto-rename-tag
  (if auto-rename-tag-mode
      (auto-rename-tag-enable)
    (auto-rename-tag-disable)))

(defun auto-rename-tag-turn-on-auto-rename-tag-mode ()
  "Turn on the 'auto-rename-tag-mode' minor mode."
  (auto-rename-tag-mode 1))

(define-globalized-minor-mode global-auto-rename-tag-mode
  auto-rename-tag-mode auto-rename-tag-turn-on-auto-rename-tag-mode
  :require 'auto-rename-tag)

(provide 'auto-rename-tag)
;;; auto-rename-tag.el ends here
