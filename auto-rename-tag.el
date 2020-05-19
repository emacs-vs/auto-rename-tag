;;; auto-rename-tag.el --- Automatically rename paired HTML/XML tag  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-12-01 23:56:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically rename paired HTML/XML tag.
;; Keyword: auto-complete html rename tag xml
;; Version: 0.2.1
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


(defconst auto-rename-tag--tag-regexp "<[^>]*"
  "Tag regular expression to find tag position.")

(defvar auto-rename-tag--pre-command-actived nil
  "Check if `pre-command-hook' called.")

(defvar auto-rename-tag--record-prev-word ""
  "Record down the word in `pre-command-hook'.")


(defun auto-rename-tag--delete-tag-name ()
  "Delete the current tag name."
  (let ((tag-start (auto-rename-tag--tag-name-start-pt))
        (tag-end (auto-rename-tag--tag-name-end-pt)))
    (delete-region tag-start tag-end)))

(defun auto-rename-tag--is-beginning-of-buffer-p ()
  "Is at the beginning of buffer?"
  (= (point) (point-min)))

(defun auto-rename-tag--is-end-of-buffer-p ()
  "Is at the end of buffer?"
  (= (point) (point-max)))

(defun auto-rename-tag--current-char-equal-p (c)
  "Check the current character equal to 'C'."
  (if (or (auto-rename-tag--is-beginning-of-buffer-p)
          (auto-rename-tag--is-end-of-buffer-p))
      nil
    (let ((current-char-string (string (char-before))))
      (string= current-char-string c))))

(defun auto-rename-tag--is-closing-tag-p ()
  "Check if current tag a closing tag."
  (save-excursion
    (when (auto-rename-tag--current-char-equal-p "<")
      (forward-char 1))
    (auto-rename-tag--current-char-equal-p "/")))

(defun auto-rename-tag--inside-tag-p ()
  "Check if current point inside the tag."
  (let ((backward-less (save-excursion (search-backward "<" nil t)))
        (backward-greater (save-excursion (search-backward ">" nil t)))
        (forward-less (save-excursion (search-forward "<" nil t)))
        (forward-greater (save-excursion (search-forward ">" nil t))))
    (unless backward-less (setq backward-less -1))
    (unless backward-greater (setq backward-greater -1))
    (unless forward-less (setq forward-less -1))
    (unless forward-greater (setq forward-greater -1))
    (and (not (= -1 backward-less))
         (not (= -1 forward-greater))
         (< backward-greater backward-less)
         (or (< forward-greater forward-less)
             (= -1 forward-less)))))

(defun auto-rename-tag--self-tag-p ()
  "Tag that can be use individually."
  (if (auto-rename-tag--inside-tag-p)
      (save-excursion
        (auto-rename-tag--goto-the-start-of-tag-name)
        (re-search-forward "/[ \t\n]*>" (auto-rename-tag--end-tag-point) t))
    nil))

(defun auto-rename-tag--goto-backward-tag ()
  "Goto the backward tag."
  (when (auto-rename-tag--inside-tag-p)
    (re-search-backward auto-rename-tag--tag-regexp nil t))
  (forward-char -1)
  (if (re-search-backward auto-rename-tag--tag-regexp nil t)
      (forward-char 1)
    (goto-char (point-min))))

(defun auto-rename-tag--goto-forward-tag ()
  "Goto the forward tag."
  (let ((search-back-result nil) (first-tag nil))
    (save-excursion
      (re-search-forward auto-rename-tag--tag-regexp nil t)
      (forward-char -1)
      (setq search-back-result (re-search-backward auto-rename-tag--tag-regexp nil t))
      (setq first-tag (null search-back-result)))
    (if first-tag
        (if (re-search-forward auto-rename-tag--tag-regexp nil t)
            (if (re-search-backward auto-rename-tag--tag-regexp nil t)
                (forward-char 1)
              (goto-char (point-max)))
          (goto-char (point-max)))
      (if (re-search-forward auto-rename-tag--tag-regexp nil t)
          (if (re-search-backward auto-rename-tag--tag-regexp nil t)
              (forward-char 1)
            (goto-char (point-max)))
        (goto-char (point-max))))))

(defun auto-rename-tag--goto-backward-tag-name (name)
  "Goto the backward tag name 'NAME'."
  (let ((current-word ""))
    (auto-rename-tag--goto-backward-tag)

    (unless (auto-rename-tag--is-beginning-of-buffer-p)
      (setq current-word (auto-rename-tag--get-tag-name-at-point))

      ;; Ensure `current-word'/name is something other than nil.
      (unless current-word (setq current-word ""))
      (unless name (setq name ""))

      (unless (string= current-word name)
        (auto-rename-tag--goto-backward-tag-name name)))))

(defun auto-rename-tag--goto-forward-tag-name (name)
  "Goto the forward tag name 'NAME'."
  (let ((current-word ""))
    (auto-rename-tag--goto-forward-tag)

    (unless (auto-rename-tag--is-end-of-buffer-p)
      (setq current-word (auto-rename-tag--get-tag-name-at-point))

      ;; Ensure `current-word'/name is something other than nil.
      (unless current-word (setq current-word ""))
      (unless name (setq name ""))

      (unless (string= current-word name)
        (auto-rename-tag--goto-forward-tag-name name)))))

(defun auto-rename-tag--backward-count-nested-close-tag (name &optional nc dnc)
  "Search backward, return the count of the nested closing tag.
NAME : target word/tag name to check nested.
NC : recursive nested count.
DNC : duplicate nested count."
  (save-excursion
    (let ((nested-count (if nc nc 0)) (dup-nested-count (if dnc dnc 0))
          (current-word "") (is-end-tag nil))
      (auto-rename-tag--goto-backward-tag)

      (unless (auto-rename-tag--is-beginning-of-buffer-p)
        (setq is-end-tag (auto-rename-tag--is-closing-tag-p))
        (setq current-word (auto-rename-tag--get-tag-name-at-point))

        ;; Ensure `current-word'/name is something other than nil.
        (unless current-word (setq current-word ""))
        (unless name (setq name ""))

        ;; If closing tag.
        (when is-end-tag
          (when (string= current-word name)
            (setq nested-count (+ nested-count 1))
            (setq dup-nested-count (+ dup-nested-count 1)))
          (setq nested-count
                (auto-rename-tag--backward-count-nested-close-tag
                 name nested-count dup-nested-count)))
        ;; If opening tag.
        (unless is-end-tag
          (unless (= dup-nested-count 0)
            (when (string= current-word name)
              (setq dup-nested-count (- dup-nested-count 1))
              (setq nested-count
                    (auto-rename-tag--backward-count-nested-close-tag
                     name nested-count dup-nested-count))))))
      nested-count)))

(defun auto-rename-tag--forward-count-nested-open-tag (name &optional nc dnc)
  "Search forward, return the count of the nested opening tag.
NAME : target word/tag name to check nested.
NC : recursive nested count.
DNC : duplicate nested count."
  (save-excursion
    (let ((nested-count (if nc nc 0)) (dup-nested-count (if dnc dnc 0))
          (current-word "") (is-end-tag nil))
      (auto-rename-tag--goto-forward-tag)

      (unless (auto-rename-tag--is-end-of-buffer-p)
        (setq is-end-tag (auto-rename-tag--is-closing-tag-p))
        (setq current-word (auto-rename-tag--get-tag-name-at-point))

        ;; Ensure `current-word'/name is something other than nil.
        (unless current-word (setq current-word ""))
        (unless name (setq name ""))

        ;; If closing tag.
        (when is-end-tag
          (unless (= dup-nested-count 0)
            (when (string= current-word name)
              (setq dup-nested-count (- dup-nested-count 1))
              (setq nested-count
                    (auto-rename-tag--forward-count-nested-open-tag
                     name nested-count dup-nested-count)))))
        ;; If opening tag.
        (unless is-end-tag
          (when (string= current-word name)
            (setq nested-count (+ nested-count 1))
            (setq dup-nested-count (+ dup-nested-count 1)))
          (setq nested-count
                (auto-rename-tag--forward-count-nested-open-tag
                 name nested-count dup-nested-count))))
      nested-count)))


(defun auto-rename-tag--start-tag-point ()
  "Return the point of the tag starting point."
  (if (auto-rename-tag--inside-tag-p)
      (save-excursion
        (search-backward "<" nil t)
        (point))
    nil))

(defun auto-rename-tag--end-tag-point ()
  "Return the point of the tag ending point."
  (if (auto-rename-tag--inside-tag-p)
      (save-excursion
        ;; TODO: There is some logic error about nested level.
        ;; Yet since, we are only going to use this for checking the
        ;; self tag. This logic error doesn't necessary has to be resolved!
        (let ((start-tag-pt (auto-rename-tag--start-tag-point))
              (nested-start-tag-pt nil))
          (re-search-forward "[^=][ \t\n]*>" nil t)
          (forward-char -1)
          (setq nested-start-tag-pt (auto-rename-tag--start-tag-point))
          (if (or (null nested-start-tag-pt)
                  (= start-tag-pt nested-start-tag-pt))
              (progn
                (forward-char 1)
                (point))
            (auto-rename-tag--end-tag-point))))
    nil))

(defun auto-rename-tag--goto-the-start-of-tag-name ()
  "Goto the start of the tag name, in order to get the name of the tag."
  (re-search-backward "[<]" nil t)
  (forward-char 1)
  (when (auto-rename-tag--is-closing-tag-p)
    (forward-char 1)))

(defun auto-rename-tag--goto-the-end-of-tag-name ()
  "Goto the end of the tag name, in order to get the name of the tag."
  (re-search-backward "[<]" nil t)
  (re-search-forward "[^> \t\n]*" nil t))

(defun auto-rename-tag--tag-name-start-pt ()
  "Return the point at the start of the tag name."
  (save-excursion
    (auto-rename-tag--goto-the-start-of-tag-name)
    (point)))

(defun auto-rename-tag--tag-name-end-pt ()
  "Return the point at the end of the tag name."
  (save-excursion
    (auto-rename-tag--goto-the-end-of-tag-name)
    (point)))

(defun auto-rename-tag--get-tag-name-at-point ()
  "Get the tag name at point."
  (let ((tag-name ""))
    (let ((tag-start (auto-rename-tag--tag-name-start-pt))
          (tag-end (auto-rename-tag--tag-name-end-pt)))
      (if (= tag-start tag-end)
          (setq tag-name "")
        (setq tag-name (buffer-substring-no-properties tag-start tag-end))))
    tag-name))


(defun auto-rename-tag--before-change-functions (_begin _end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."

  ;; Reset record.
  (setq auto-rename-tag--record-prev-word "")
  ;; Reset flag.
  (setq auto-rename-tag--pre-command-actived nil)

  (when (and (not undo-in-progress) (not (auto-rename-tag--self-tag-p)))
    (save-excursion
      ;; Set active flag.
      (setq auto-rename-tag--pre-command-actived t)

      (setq auto-rename-tag--record-prev-word (auto-rename-tag--get-tag-name-at-point))

      (when (string= auto-rename-tag--record-prev-word "/")
        (setq auto-rename-tag--record-prev-word ""))

      ;; Ensure `auto-rename-tag--record-prev-word' is something other than nil.
      (unless auto-rename-tag--record-prev-word
        (setq auto-rename-tag--record-prev-word "")))))

(defun auto-rename-tag--after-change-function (_begin _end _length)
  "Do stuff after buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes.
LENGTH : deletion length."
  (when (and auto-rename-tag--pre-command-actived (not (auto-rename-tag--self-tag-p)))
    (save-excursion
      (let ((is-end-tag nil)
            (current-word "") (pair-tag-word "")
            (nested-count 0))
        ;; Goto the first character inside the tag.
        (auto-rename-tag--goto-the-start-of-tag-name)

        (setq is-end-tag (auto-rename-tag--is-closing-tag-p))

        (setq current-word (auto-rename-tag--get-tag-name-at-point))

        (unless (string= current-word auto-rename-tag--record-prev-word)
          ;; NOTE: Is closing tag.
          (when is-end-tag
            ;; Get nested count.
            (setq nested-count
                  (auto-rename-tag--backward-count-nested-close-tag auto-rename-tag--record-prev-word))

            ;; Resolve nested.
            (while (not (= nested-count 0))
              (setq nested-count (- nested-count 1))
              (auto-rename-tag--goto-backward-tag-name auto-rename-tag--record-prev-word)
              (auto-rename-tag--goto-backward-tag-name auto-rename-tag--record-prev-word))

            ;; Goto the target pair.
            (auto-rename-tag--goto-backward-tag-name auto-rename-tag--record-prev-word)

            ;; Get the tag name and ready to be compare.
            (setq pair-tag-word (auto-rename-tag--get-tag-name-at-point))

            ;; Ensure `pair-tag-word' is something other than nil.
            (unless pair-tag-word (setq pair-tag-word ""))

            (when (string= auto-rename-tag--record-prev-word pair-tag-word)
              ;; Delete the pair word.
              (unless (string= pair-tag-word "")
                (auto-rename-tag--delete-tag-name))
              ;; Insert new word.
              (insert current-word)))

          ;; NOTE: Is opening tag.
          (unless is-end-tag
            ;; Get nested count.
            (setq nested-count
                  (auto-rename-tag--forward-count-nested-open-tag auto-rename-tag--record-prev-word))

            ;; Resolve nested.
            (while (not (= nested-count 0))
              (setq nested-count (- nested-count 1))
              (auto-rename-tag--goto-forward-tag-name auto-rename-tag--record-prev-word)
              (auto-rename-tag--goto-forward-tag-name auto-rename-tag--record-prev-word))

            ;; Goto the target pair.
            (auto-rename-tag--goto-forward-tag-name auto-rename-tag--record-prev-word)

            (ignore-errors (forward-char 1))

            ;; Get the tag name and ready to be compare.
            (setq pair-tag-word (auto-rename-tag--get-tag-name-at-point))

            ;; Ensure `pair-tag-word' is something other than nil.
            (unless pair-tag-word (setq pair-tag-word ""))

            (when (string= auto-rename-tag--record-prev-word pair-tag-word)
              ;; Delete the pair word.
              (unless (string= pair-tag-word "")
                (auto-rename-tag--delete-tag-name))
              ;; Insert new word.
              (insert current-word))))))))


(defun auto-rename-tag--enable ()
  "Enable `auto-rename-tag' in current buffer."
  (add-hook 'before-change-functions #'auto-rename-tag--before-change-functions nil t)
  (add-hook 'after-change-functions #'auto-rename-tag--after-change-function nil t))

(defun auto-rename-tag--disable ()
  "Disable `auto-rename-tag' in current buffer."
  (remove-hook 'before-change-functions #'auto-rename-tag--before-change-functions t)
  (remove-hook 'after-change-functions #'auto-rename-tag--after-change-function t))


;;;###autoload
(define-minor-mode auto-rename-tag-mode
  "Minor mode 'auto-rename-tag' mode."
  :lighter " ART"
  :group auto-rename-tag
  (if auto-rename-tag-mode
      (auto-rename-tag--enable)
    (auto-rename-tag--disable)))

(defun auto-rename-tag-turn-on-auto-rename-tag-mode ()
  "Turn on the 'auto-rename-tag-mode' minor mode."
  (auto-rename-tag-mode 1))

;;;###autoload
(define-globalized-minor-mode global-auto-rename-tag-mode
  auto-rename-tag-mode auto-rename-tag-turn-on-auto-rename-tag-mode
  :require 'auto-rename-tag)

(provide 'auto-rename-tag)
;;; auto-rename-tag.el ends here
