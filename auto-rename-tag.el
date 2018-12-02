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

(defun auto-rename-tag-goto-backward-tag-name (name)
  "Goto the backward tag name 'NAME'."
  (let ((is-end-tag nil)
        (current-word ""))
    (auto-rename-tag-goto-backward-tag)
    (forward-char 1)

    (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
    (when is-end-tag
      (forward-char 1))
    (setq current-word (thing-at-point 'word))

    (if current-word
        (if (not (string= current-word name))
            (auto-rename-tag-goto-backward-tag-name name)
          (backward-char 1))
      (if (or (not name)  ;; If, nil.
              (string= name ""))
          (backward-char 1)
        (auto-rename-tag-goto-backward-tag-name name)))))

(defun auto-rename-tag-goto-forward-tag-name (name)
  "Goto the forward tag name 'NAME'."
  (let ((is-end-tag nil)
        (current-word ""))
    (auto-rename-tag-goto-forward-tag)
    (forward-char 1)

    (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
    (when is-end-tag
      (forward-char 1))
    (setq current-word (thing-at-point 'word))

    (if current-word
        (if (not (string= current-word name))
            (auto-rename-tag-goto-forward-tag-name name)
          (backward-char 1))
      (if (or (not name)  ;; If, nil.
              (string= name ""))
          (backward-char 1)
        (auto-rename-tag-goto-forward-tag-name name)))))

(defun auto-rename-tag-backward-count-nested-close-tag (name &optional nc)
  "Search backward, return the count of the nested closing tag.
NAME : target word/tag name to check nested.
NC : recursive nested count."
  (save-excursion
    (let ((nested-count 0)
          (current-word "")
          (is-end-tag nil))
      (when nc
        (setq nested-count nc))

      (auto-rename-tag-goto-backward-tag)

      (when (not (auto-rename-tag-is-beginning-of-buffer-p))
        (forward-char 1)

        (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
        (when is-end-tag
          (forward-char 1))
        ;; If outside of tag, go back then.
        (when (not (auto-rename-tag-inside-tag))
          (backward-char 1))
        (setq current-word (thing-at-point 'word))

        ;; Ensure current-word/name is something other than nil.
        (when (not current-word)
          (setq current-word ""))
        (when (not name)
          (setq name ""))

        ;; If closing tag.
        (if is-end-tag
            (progn
              (when (string= current-word name)
                (setq nested-count (+ nested-count 1)))
              (setq nested-count (auto-rename-tag-backward-count-nested-close-tag name nested-count)))
          ;; If opening tag.
          (when (not (string= current-word name))
            (setq nested-count (auto-rename-tag-backward-count-nested-close-tag name nested-count)))))
      nested-count)))

(defun auto-rename-tag-forward-count-nested-open-tag (name &optional nc)
  "Search forward, return the count of the nested opening tag.
NAME : target word/tag name to check nested.
NC : recursive nested count."
  (save-excursion
    (let ((nested-count 0)
          (current-word "")
          (is-end-tag nil))
      (when nc
        (setq nested-count nc))

      (auto-rename-tag-goto-forward-tag)

      (when (not (auto-rename-tag-is-end-of-buffer-p))
        (forward-char 1)

        (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
        (when is-end-tag
          (forward-char 1)
          ;; If end of tag, go back then.
          (when (not (auto-rename-tag-inside-tag))
            (backward-char 1)))
        (setq current-word (thing-at-point 'word))

        ;; Ensure current-word/name is something other than nil.
        (when (not current-word)
          (setq current-word ""))
        (when (not name)
          (setq name ""))

        ;; If closing tag.
        (if is-end-tag
            (when (not (string= current-word name))
              (setq nested-count (auto-rename-tag-forward-count-nested-open-tag name nested-count)))
          ;; If opening tag.
          (progn
            (when (string= current-word name)
              (setq nested-count (+ nested-count 1)))
            (setq nested-count (auto-rename-tag-forward-count-nested-open-tag name nested-count)))))
      nested-count)))


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
            (is-end-tag nil)
            (current-word "")
            (pair-tag-word "")
            (nested-count 0))

        ;; Goto the first character inside the tag.
        (setq first-char-point-in-tag (+ (auto-rename-tag-backward-char-at-point "<") 1))
        (goto-char first-char-point-in-tag)

        (cond ((auto-rename-tag-current-char-equal-p "/")
               ;; If the first char is '/', meaning is the end tag.
               (setq is-end-tag t))
              ((auto-rename-tag-current-char-equal-p ">")
               ;; If this is true, meaning the tag is empty.
               (backward-char 1)))

        ;; Get the current word, if not nil.
        (when (thing-at-point 'word)
          (setq current-word (thing-at-point 'word)))

        (when (not (string= current-word auto-rename-tag-record-prev-word))
          ;; Is closing tag.
          (if is-end-tag
              (progn
                ;; Get nested count.
                (setq nested-count
                      (auto-rename-tag-backward-count-nested-close-tag auto-rename-tag-record-prev-word))

                ;; Resolve nested.
                (while (not (= nested-count 0))
                  (setq nested-count (- nested-count 1))
                  (auto-rename-tag-goto-backward-tag-name auto-rename-tag-record-prev-word)
                  (auto-rename-tag-goto-backward-tag-name auto-rename-tag-record-prev-word))

                ;; Goto the target pair.
                (auto-rename-tag-goto-backward-tag-name auto-rename-tag-record-prev-word)

                ;; Get the tag name and ready to be compare.
                (setq pair-tag-word (thing-at-point 'word))

                (message "PTW: %s" pair-tag-word)

                (when (string= auto-rename-tag-record-prev-word pair-tag-word)
                  ;; Delete the pair word.
                  ;;(auto-rename-tag-delete-word 1)
                  (insert "o")

                  ;; Insert new word.
                  ;;(insert current-word)
                  ))
            ;; Is opening tag.
            (progn
              ;; Get nested count.
              (setq nested-count
                    (auto-rename-tag-forward-count-nested-open-tag auto-rename-tag-record-prev-word))

              ;; Resolve nested.
              (while (not (= nested-count 0))
                (setq nested-count (- nested-count 1))
                (auto-rename-tag-goto-forward-tag-name auto-rename-tag-record-prev-word)
                (auto-rename-tag-goto-forward-tag-name auto-rename-tag-record-prev-word))

              ;; Goto the target pair.
              (auto-rename-tag-goto-forward-tag-name auto-rename-tag-record-prev-word)

              ;; Get the tag name and ready to be compare.
              (setq pair-tag-word (thing-at-point 'word))

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
