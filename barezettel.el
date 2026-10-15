;;; barezettel.el --- Minimalistic Zettelkasten minor mode for Denote -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: J Wayston
;; Maintainer: J Wayston
;; URL: https://github.com/jwayston/barezettel
;; Version: 0.1.1
;; Package-Requires: ((emacs "31.1") (denote "4.2.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides `barezettel-mode`, which enhances Denote files with
;; hanging indents, custom fontification, and Hyperbole buttons for bare Denote
;; IDs. The core idea is to allow keep using Denote without visual clutter
;; that Org links and =denote:= prefixes may cause.
;;
;; The package relies on Denote and Hyperbole

;;; Code:

(require 'org)
(require 'denote)

(defgroup barezettel nil
  "Minimalistic Zettelkasten minor mode for Denote."
  :prefix "barezettel--"
  :group 'data)

(defcustom barezettel-use-brackets-around-signature t
  "Non-nil means `barezettel-mode' will add brackets around signature when
handling links."
  :type 'boolean
  :group 'barezettel)

(defcustom barezettel-denote-regexp "[0-9]\\{8\\}T[0-9]\\{6\\}"
  "Regexp for Denote ID"
  :type 'string
  :group 'barezettel)

(defface barezettel--denote-id-face '((t (:inherit shadow)))
  "Face for bare Denote IDs"
  :group 'barezettel)

(defvar-keymap barezettel-denote-id-keymap
  :doc "Keymap for barezettel ID buttons."
  ;; "M-RET" #'barezettel-denote-show-targets-at-point
  ;; "M-<return>" #'barezettel-denote-show-targets-at-point
  )

(defvar barezettel--font-lock-keywords
  `((,barezettel-denote-regexp
     (0 `(face barezettel--denote-id-face
	  keymap ,barezettel-denote-id-keymap
	  help-echo barezettel--denote-id-help-echo)
        t)))
  "Keywords for barezettel-mode font-lock")

;;;; Hanging indent for bare Denote id's
(defun barezettel--jit-apply-denote-indent (beg end)
  "Find Denote IDs between BEG and END and lock them with wrap prefix.
If there is anything after the id that ends with two spaces (signature), start
hanging indent after the spaces instead of at the end of the id."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (forward-line 0)
      (while (and (< (point) end) (not (eobp)))
        (if (looking-at "^\\s-*\\([0-9]\\{8\\}T[0-9]\\{6\\}:?\\s-*\\)\\([^[:space:]]+\\s-\\{2\\}\\)?")
            (let* ((mbeg (match-beginning 0))
                   (mend (match-end 0))
                   (prefix (make-string (- mend mbeg) ?\s)))
              (put-text-property (match-beginning 1) (line-end-position) 'wrap-prefix prefix))
          ;; If no ID is detected, remove possible old prefix
          (remove-text-properties (line-beginning-position) (line-end-position) '(wrap-prefix nil)))
        (forward-line 1))))
  (list beg end))

(defun barezettel--denote-indent-toggle (enable)
  "Toggle bare Denote ID hanging indent behavior based on ENABLE"
  (interactive)
  (if enable
      (progn
        (visual-line-mode 1)   ; We need visual line mode to make indenting work
        (jit-lock-register #'barezettel--jit-apply-denote-indent nil))
    (jit-lock-unregister #'barezettel--jit-apply-denote-indent)
    (with-silent-modifications		; Do not mess undo history
      (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))
  (jit-lock-refontify))

;;;; Interactive functions
(defun barezettel--format-signature (signature)
  "Format signature per `barezettel-use-brackets-around-signature'."
  (if (and signature (y-or-n-p "Include signature?"))
      (if barezettel-use-brackets-around-signature (format "(%s)  " signature)
	(format "%s  " signature))
    ""))

(defun barezettel-forward-id ()
  (interactive)
  (let ((pos (point))
        (found nil)
        (target-face 'barezettel--denote-id-face))
    (while (and (not found) (setq pos (next-single-property-change pos 'face)))
      (let ((prop (get-text-property pos 'face)))
        (when (or (eq prop target-face) (and (listp prop) (memq target-face prop)))
          (setq found pos))))
    (if found (goto-char found) (message "No more IDs found"))))

(defun barezettel-backward-id ()
  (interactive)
  (let ((pos (point))
        (found nil)
        (target-face 'barezettel--denote-id-face))
    (while (and (not found) (setq pos (previous-single-property-change pos 'face)))
      (let ((prop (get-text-property (1- pos) 'face)))
        (when (or (eq prop target-face) (and (listp prop) (memq target-face prop)))
          (setq found (previous-single-property-change pos 'face nil (point-min))))))
    (if found (goto-char found) (message "No more IDs found"))))


(defun barezettel-denote-org-link-dismantle ()
  "Dismantle Org Denote link to: DENOTEID Signature  Description
The signature part is optional and will be skipped if not found."
  (interactive)
  (if (org-in-regexp "\\[\\[denote:\\([^]]+\\)\\]\\[\\(?:\\([^ ]+\\)[ ]\\{2\\}\\)?\\([^]]+\\)\\]\\]")
      (let ((signature (match-string 2)))
        (replace-match (format "%s %s%s"
                               (match-string 1)
                               (barezettel--format-signature signature)
                               (match-string 3)) t t))
    (user-error "Cursor must be over an Org Denote link")))

(defun barezettel-denote-append-title-at-point (arg)
  "Append title after the bare Denote ID. With universal argument, prepend
title before the ID."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (id (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (path (denote-get-path-by-id id))
         (file-type (denote-filetype-heuristics path))
         (title (denote-retrieve-title-or-filename path file-type))
         (signature (denote-retrieve-filename-signature path)))
    (unless title (user-error "Couldn't retrieve title"))
    (save-excursion
      (let ((prepend (equal arg '(4))))
        (goto-char (if prepend (car bounds) (cdr bounds)))
        (unless prepend (insert " "))
        (insert (barezettel--format-signature signature) title (if prepend " " ""))))))

(defun barezettel-denote-link ()
  "Barezettel replacement for denote-link."
  (interactive)
  (save-excursion
    (let ((current-prefix-arg '(4)))
      (call-interactively 'denote-link))
    (if (org-in-regexp "\\[\\[denote:\\(.*?\\)\\].*\\]")
	(let ((id (match-string 1)))
	  (replace-match (format "%s" id)))
      (user-error "Link format error."))
    (call-interactively 'barezettel-denote-append-title-at-point)))

;;;; Denote core functionality modifications and additions
;;;;; Allow bare Denote IDs to be registered as backlinks
(defun barezettel--denote-retrieve-xref-alist-for-backlinks (orig-fn identifier)
  "Include raw occurrences of Denote IDs in Denote backlinks.
ORIG-FN is the original backlink retrieval function."
  (let* ((original (funcall orig-fn identifier))
	 (raw (denote-retrieve-xref-alist identifier))
	 (seen (mapcar #'car original)))
    (append original (seq-remove (lambda (entry) (member (car entry) seen)) raw))))

(defun barezettel-denote-show-targets-at-point (arg)
  "Show completing list of all the notes containing ID. With universal
argument, open in another window."
  (interactive "P")
  (let ((id (save-excursion
              (skip-chars-backward "0-9T")
              (when (looking-at barezettel-denote-regexp)
                (match-string-no-properties 0)))))
    (if (not id)
        (message "No ID recognized at point")
      (let ((current-file (denote-get-path-by-id id)))
        (if (not current-file)
            (message "Couldn't find Denote file for ID: %s" id)
          (let* ((backlinks (ignore-errors (denote-get-backlinks current-file)))
                 (all-files (delete-dups (cons current-file backlinks)))
                 (choices (mapcar (lambda (f)
                                    (cons (file-relative-name f (car (denote-directories))) f))
                                  all-files))
                 (default-choice (car (car choices)))
                 (selected (completing-read "ID was found in notes: " choices nil t nil nil default-choice)))
            (when-let* ((path (cdr (assoc selected choices))))
              (if (equal arg '(4))
		  (find-file-other-window path)
		(find-file path)))))))))


;;;; Font lock rules for bare Denote IDs
(defun barezettel--denote-id-help-echo (_window _object position)
  "Return Denote filename for the Denote ID at POSITION."
  (save-excursion
    (goto-char position)
    (skip-chars-backward "0-9T")
    (when (looking-at barezettel-denote-regexp)
      (file-name-nondirectory (denote-get-path-by-id (match-string-no-properties 0))))))

(defun barezettel--setup-font-lock (enable)
  "Enable or disable barezettel-mode font-lock rules based on ENABLE."
  (if enable
      (progn
        (add-to-list 'font-lock-extra-managed-props 'help-echo)
        (add-to-list 'font-lock-extra-managed-props 'keymap)

        (font-lock-add-keywords nil barezettel--font-lock-keywords 'prepend))
    (font-lock-remove-keywords nil barezettel--font-lock-keywords))
  (font-lock-flush))


;;;###autoload
(define-minor-mode barezettel-mode
  "Toggles barezettel-mode."
  :init-value nil
  :lighter " BZ"
  (barezettel--setup-font-lock barezettel-mode)
  (barezettel--denote-indent-toggle barezettel-mode)
  (if barezettel-mode
      (progn
	(advice-add 'denote-retrieve-xref-alist-for-backlinks
		    :around #'barezettel--denote-retrieve-xref-alist-for-backlinks))
    (progn
      (advice-remove 'denote-retrieve-xref-alist-for-backlinks
                     #'barezettel--denote-retrieve-xref-alist-for-backlinks))))

(provide 'barezettel)
