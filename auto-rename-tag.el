;;; auto-rename-tag.el --- Automatically rename paired HTML/XML tag.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-12-01 23:56:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically rename paired HTML/XML tag.
;; Keyword: auto-complete html rename tag xml
;; Version: 0.0.2
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
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/auto-rename-tag"))


(defvar auto-rename-tag-pre-command-actived nil
  "Check if `pre-command-hook' called.")

(defvar auto-rename-tag-record-prev-word ""
  "Record down the word in `pre-command-hook'.")


(defun auto-rename-tag-delete-tag-name ()
  "Delete the current tag name."
  (let ((st-pt (point))
        (end-pt -1))
    (auto-rename-tag-goto-the-end-of-tag-name)
    (setq end-pt (point))
    (delete-region st-pt end-pt)))

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
  (unless (auto-rename-tag-is-end-of-buffer-p)
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

    (unless (auto-rename-tag-is-beginning-of-buffer-p)
      (forward-char 1)

      (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
      (when is-end-tag
        (forward-char 1))
      ;; If outside of tag, go back then.
      (unless (auto-rename-tag-inside-tag)
        (backward-char 1))

      (save-excursion
        (backward-char 1)
        (setq current-word (auto-rename-tag-get-tag-name-at-point)))

      ;; Ensure `current-word'/name is something other than nil.
      (unless current-word (setq current-word ""))
      (unless name (setq name ""))

      (if current-word
          (if (not (string= current-word name))
              (auto-rename-tag-goto-backward-tag-name name)
            (backward-char 1))
        (if (string= name "")
            (backward-char 1)
          (auto-rename-tag-goto-backward-tag-name name))))))

(defun auto-rename-tag-goto-forward-tag-name (name)
  "Goto the forward tag name 'NAME'."
  (let ((is-end-tag nil)
        (current-word ""))
    (auto-rename-tag-goto-forward-tag)

    (unless (auto-rename-tag-is-end-of-buffer-p)
      (forward-char 1)

      (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
      (when is-end-tag
        (forward-char 1)
        ;; If end of tag, go back then.
        (unless (auto-rename-tag-inside-tag)
          (backward-char 1)))

      (save-excursion
        (unless (auto-rename-tag-current-char-equal-p "/")
          (backward-char 1))
        (setq current-word (auto-rename-tag-get-tag-name-at-point)))

      ;; Ensure `current-word'/name is something other than nil.
      (unless current-word (setq current-word ""))
      (unless name (setq name ""))

      (if current-word
          (if (not (string= current-word name))
              (auto-rename-tag-goto-forward-tag-name name)
            (backward-char 1))
        (if (string= name "")
            (backward-char 1)
          (auto-rename-tag-goto-forward-tag-name name))))))

(defun auto-rename-tag-backward-count-nested-close-tag (name &optional nc dnc)
  "Search backward, return the count of the nested closing tag.
NAME : target word/tag name to check nested.
NC : recursive nested count.
DNC : duplicate nested count."
  (save-excursion
    (let ((nested-count 0)
          (dup-nested-count 0)
          (current-word "")
          (is-end-tag nil))
      (when nc
        (setq nested-count nc))
      (when dnc
        (setq dup-nested-count dnc))

      (auto-rename-tag-goto-backward-tag)

      (unless (auto-rename-tag-is-beginning-of-buffer-p)
        (forward-char 1)

        (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
        (when is-end-tag
          (forward-char 1))
        ;; If outside of tag, go back then.
        (unless (auto-rename-tag-inside-tag)
          (backward-char 1))

        (save-excursion
          (backward-char 1)
          (setq current-word (auto-rename-tag-get-tag-name-at-point)))

        ;; Ensure `current-word'/name is something other than nil.
        (unless current-word (setq current-word ""))
        (unless name (setq name ""))

        ;; If closing tag.
        (if is-end-tag
            (progn
              (when (string= current-word name)
                (setq nested-count (+ nested-count 1))
                (setq dup-nested-count (+ dup-nested-count 1)))
              (setq nested-count
                    (auto-rename-tag-backward-count-nested-close-tag name
                                                                     nested-count
                                                                     dup-nested-count)))
          ;; If opening tag.
          (unless (= dup-nested-count 0)
            (when (string= current-word name)
              (setq dup-nested-count (- dup-nested-count 1))
              (setq nested-count
                    (auto-rename-tag-backward-count-nested-close-tag name
                                                                     nested-count
                                                                     dup-nested-count))))))
      nested-count)))

(defun auto-rename-tag-forward-count-nested-open-tag (name &optional nc dnc)
  "Search forward, return the count of the nested opening tag.
NAME : target word/tag name to check nested.
NC : recursive nested count.
DNC : duplicate nested count."
  (save-excursion
    (let ((nested-count 0)
          (dup-nested-count 0)
          (current-word "")
          (is-end-tag nil))
      (when nc
        (setq nested-count nc))
      (when dnc
        (setq dup-nested-count dnc))

      (auto-rename-tag-goto-forward-tag)

      (unless (auto-rename-tag-is-end-of-buffer-p)
        (forward-char 1)

        (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
        (when is-end-tag
          (forward-char 1)
          ;; If end of tag, go back then.
          (unless (auto-rename-tag-inside-tag)
            (backward-char 1)))

        (save-excursion
          (backward-char 1)
          (setq current-word (auto-rename-tag-get-tag-name-at-point)))

        ;; Ensure `current-word'/name is something other than nil.
        (unless current-word (setq current-word ""))
        (unless name (setq name ""))

        ;; If closing tag.
        (if is-end-tag
            (unless (= dup-nested-count 0)
              (when (string= current-word name)
                (setq dup-nested-count (- dup-nested-count 1))
                (setq nested-count
                      (auto-rename-tag-forward-count-nested-open-tag name
                                                                     nested-count
                                                                     dup-nested-count))))
          ;; If opening tag.
          (progn
            (when (string= current-word name)
              (setq nested-count (+ nested-count 1))
              (setq dup-nested-count (+ dup-nested-count 1)))
            (setq nested-count
                  (auto-rename-tag-forward-count-nested-open-tag name
                                                                 nested-count
                                                                 dup-nested-count)))))
      nested-count)))

(defun auto-rename-tag-goto-the-end-of-tag-name ()
  "Goto the end of the tag name, in order to get the name of the tag."
  (unless (auto-rename-tag-is-end-of-buffer-p)
    (forward-char 1))
  (while (and (not (auto-rename-tag-is-end-of-buffer-p))
              (not (auto-rename-tag-current-char-equal-p " "))
              (not (auto-rename-tag-current-char-equal-p "\t"))
              (not (auto-rename-tag-current-char-equal-p ">")))
    (forward-char 1))
  (backward-char 1))

(defun auto-rename-tag-get-tag-name-at-point ()
  "Get the tag name at point."
  (let ((tag-name ""))
    (save-excursion
      (let ((tag-start (point))
            (tag-end -1))
        (auto-rename-tag-goto-the-end-of-tag-name)
        (setq tag-end (point))
        (setq tag-name (buffer-substring-no-properties tag-start tag-end))))
    (when (string= tag-name "/")
      (setq tag-name ""))
    tag-name))


(defun auto-rename-tag-before-change-functions (begin end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."

  ;; Reset record.
  (setq auto-rename-tag-record-prev-word "")
  ;; Reset flag.
  (setq auto-rename-tag-pre-command-actived nil)

  (when (and (not undo-in-progress)
             (auto-rename-tag-inside-tag))
    (save-excursion
      (let ((is-end-tag nil))
        ;; Set active flag.
        (setq auto-rename-tag-pre-command-actived t)

        ;; Record it down.
        (unless (auto-rename-tag-current-char-equal-p "<")
          (auto-rename-tag-backward-goto-char "<"))
        (forward-char 1)

        (setq is-end-tag (auto-rename-tag-current-char-equal-p "/"))
        (when is-end-tag
          (forward-char 1)
          ;; If end of tag, go back then.
          (unless (auto-rename-tag-inside-tag)
            (backward-char 1)))

        ;; NOTE: Get the tag name here.
        (save-excursion
          (backward-char 1)
          (setq auto-rename-tag-record-prev-word (auto-rename-tag-get-tag-name-at-point)))

        (when (string= auto-rename-tag-record-prev-word "/")
          (setq auto-rename-tag-record-prev-word ""))

        ;; Ensure `auto-rename-tag-record-prev-word' is something other than nil.
        (unless auto-rename-tag-record-prev-word
          (setq auto-rename-tag-record-prev-word ""))))))

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

        (unless is-end-tag
          (backward-char 1))
        (setq current-word (auto-rename-tag-get-tag-name-at-point))

        (unless (string= current-word auto-rename-tag-record-prev-word)
          ;; NOTE: Is closing tag.
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
                (setq pair-tag-word (auto-rename-tag-get-tag-name-at-point))

                ;; Ensure `pair-tag-word' is something other than nil.
                (unless pair-tag-word (setq pair-tag-word ""))

                (when (string= auto-rename-tag-record-prev-word pair-tag-word)
                  ;; Delete the pair word.
                  (unless (string= pair-tag-word "")
                    (auto-rename-tag-delete-tag-name))

                  ;; Insert new word.
                  (insert current-word)))
            ;; NOTE: Is opening tag.
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
              (setq pair-tag-word (auto-rename-tag-get-tag-name-at-point))

              ;; Ensure `pair-tag-word' is something other than nil.
              (unless pair-tag-word (setq pair-tag-word ""))

              (when (string= auto-rename-tag-record-prev-word pair-tag-word)
                ;; Delete the pair word.
                (if (string= pair-tag-word "")
                    (forward-char 1)
                  (auto-rename-tag-delete-tag-name))

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


;;;###autoload
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

;;;###autoload
(define-globalized-minor-mode global-auto-rename-tag-mode
  auto-rename-tag-mode auto-rename-tag-turn-on-auto-rename-tag-mode
  :require 'auto-rename-tag)

(provide 'auto-rename-tag)
;;; auto-rename-tag.el ends here
