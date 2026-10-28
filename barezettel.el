;;; barezettel.el --- Minimalistic Zettelkasten minor mode for Denote -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: J Wayston
;; Maintainer: J Wayston
;; URL: https://github.com/jwayston/barezettel
;; Version: 0.3.0
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

;; The core idea of this package is to allow to use Denote without
;; visual clutter that Org links and =denote:= prefixes cause when
;; there is no rendering wanted or available.  I.e. use bare Denote
;; IDs in your notes and they act as links.  This makes dealing with
;; bare text note files more visually pleasant especially when used
;; also outside Emacs.
;;
;; The package relies on Denote package.

;;; Code:

(require 'org)
(require 'denote)

;;;; Customizations
(defgroup barezettel nil
  "Minimalistic Zettelkasten minor mode for Denote."
  :prefix "barezettel--"
  :group 'data)

(defcustom barezettel-visual-hanging-indents nil
  "Non-nil means `barezettel-mode' will apply visual hanging indents."
  :type 'boolean
  :group 'barezettel)

(defcustom barezettel-use-brackets-around-signature t
  "Non-nil means signatures are formatted with brackets around them."
  :type 'boolean
  :group 'barezettel)

(defcustom barezettel-show-targets-in-denote-dired nil
  "Non-nil means open the ID action target list in a `denote-dired' buffer."
  :type 'boolean
  :group 'barezettel)

(defcustom barezettel-denote-regexp "[0-9]\\{8\\}T[0-9]\\{6\\}"
  "Regexp for Denote ID."
  :type 'string
  :group 'barezettel)

(defcustom barezettel-denote-paragraph-start-regexp "[0-9]\\{8\\}T[0-9]\\{6\\}.*?  "
  "Regexp for paragraphs starting with Denote IDs."
  :type 'string
  :group 'barezettel)

(defface barezettel--denote-id-face '((t (:inherit shadow)))
  "Face for bare Denote IDs."
  :group 'barezettel)

(defvar-keymap barezettel-mode-map
  :doc "Keymap for `barezettel-mode'.")


;;;; TOC
(defun barezettel--line-to-toc-fill-dots (fill-count)
  "Fill FILL-COUNT dots before the last word of the line."
  (end-of-line) (backward-word) (insert-char ?. fill-count) (insert " "))

(defun barezettel--line-to-toc (threshold indent)
  "Generate a nice TOC line with INDENT no longer than THRESHOLD."
  (interactive (list (read-number "Threshold: " (current-fill-column)) (read-number "Indent: " 0)))
  (forward-line 0)
  (insert-char ?\s indent)

  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (llen (string-width (buffer-substring-no-properties beg end))))

    (if (< llen threshold) ; Short line, just place the dots
        (barezettel--line-to-toc-fill-dots (- threshold 1 llen))

      ;; Longer lines
      (let* ((lword (save-excursion (goto-char end) (current-word)))
             (lwlen (length lword))
             (end-marker (copy-marker (- end lwlen)))
             (fill-column (- threshold lwlen 1)))

        (save-excursion
          ;; Remove the last word (the TOC target)
          (save-excursion (goto-char end) (delete-region (- (point) lwlen) (point)))

          ;; Wrap the line according to calculated 'fill-column'
          (fill-region beg end-marker)

          (forward-line 1)
          (let ((rbeg (line-beginning-position))
                (fill-column (- threshold lwlen 3)))

            ;; Join the remaining lines
            (let ((fill-column 999999)) (fill-region rbeg end-marker))

            (let ((rbeg (line-beginning-position)))
              ;; Rewrap rest of the text with hanging indent
              (fill-region rbeg end-marker)
              (indent-rigidly rbeg end-marker 2)
              (goto-char end-marker) (insert " " lword))

            ;; Place the dots in the last line
            (let ((dotsn (- threshold 1 (- (line-end-position)
                                           (line-beginning-position)))))
              (barezettel--line-to-toc-fill-dots dotsn))))
        (goto-char end-marker)))))

(defun barezettel--lines-to-toc (threshold indent)
  "Multiline paragraph to lines of TOC-entries of INDENT no wider than THRESHOLD."
  (interactive (list (read-number "Threshold: " 80) (read-number "Indent: " 0)))
  (let ((pbeg (save-excursion (backward-paragraph) (forward-line 1) (point)))
        (pend (save-excursion (forward-paragraph) (point))))
    (save-restriction
      (narrow-to-region pbeg pend)
      (goto-char (point-min))
      (while (not (eobp))
        (barezettel--line-to-toc threshold indent)
        (forward-line 1)))))

(defun barezettel-denote-link-toc (arg)
  "Insert denote link as a TOC entry, ARG meaning unversal argument."
  (interactive "P")
  (atomic-change-group
    (let ((defaults (equal arg '(4)))
          (current-prefix-arg '(4)))
      (call-interactively 'barezettel-denote-link)
      (if defaults
          (barezettel--line-to-toc 80 0)
        (call-interactively 'barezettel--line-to-toc)))))


;;;; Hanging indent for bare Denote id's
(defvar-local barezettel--original-visual-line-mode nil
  "Original `visual-line-mode' state.")

(defvar-local barezettel--original-visual-wrap-prefix-mode nil
  "Original `visual-wrap-prefix-mode' state.")

(defun barezettel--fill-context-prefix-override (orig-fun from to &rest args)
  "Calculates text until two spaces and forces it as a visual prefix.
If no such text is found, call ORIG-FUN with args FROM, TO, ARGS."
  (save-excursion
    (goto-char from)
    (if (looking-at "^\\([^\n]+?  \\)")
        (make-string (length (match-string 1)) ?\s)
      (apply orig-fun from to args))))

(defun barezettel--toggle-visual-hanging-indents (enable)
  "Toggle visual hanging indents based on ENABLE.

Any non-nil value for ENABLE enables functionality, nil purges the
setup.  However, enabling requires the variable
`barezettel-visual-hanging-indents' to be non-nil.

This feature is based on `visual-line-mode' and
`visual-wrap-prefix-mode' which are turned on when enabled."
  (if enable
      (when barezettel-visual-hanging-indents
        ;; Store originals
        (setq-local barezettel--original-visual-line-mode visual-line-mode)
        (setq-local barezettel--original-visual-wrap-prefix-mode (bound-and-true-p visual-wrap-prefix-mode))

        ;; Setup the behavior
        (advice-add 'fill-context-prefix :around #'barezettel--fill-context-prefix-override)
        (visual-line-mode t)
        (visual-wrap-prefix-mode t))

    (when barezettel-visual-hanging-indents
      ;; Purge setup
      (advice-remove 'fill-context-prefix #'barezettel--fill-context-prefix-override)
      (visual-line-mode barezettel--original-visual-line-mode)
      (visual-wrap-prefix-mode barezettel--original-visual-wrap-prefix-mode))))

;;;; Interactive functions
(defun barezettel--format-signature (signature)
  "Format SIGNATURE per `barezettel-use-brackets-around-signature'.

If SIGNATURE was nil or user answered no, return nil."
  (if (and signature (y-or-n-p "Include signature?"))
      (if barezettel-use-brackets-around-signature
          (format "(%s)" signature)
        (format "%s" signature))
    nil))

(defun barezettel-forward-id ()
  "Jump to a next ID."
  (interactive)
  (let (match)
    (save-excursion
      (forward-word) (forward-char)
      (setq match (text-property-search-forward 'face 'barezettel--denote-id-face t)))
    (if (not match) (message "No more IDs ahead")
      (goto-char (prop-match-beginning match)))))

(defun barezettel-backward-id ()
  "Jump to a previous ID."
  (interactive)
  (let (match)
    (save-excursion
      (unless (looking-at "^\\|$\\|\\s\\|\\b\\w")
        (backward-word))
      (setq match (text-property-search-backward 'face 'barezettel--denote-id-face t)))
    (if (not match) (message "No more IDs before")
      (goto-char (prop-match-beginning match)))))


(defun barezettel-denote-org-link-dismantle ()
  "Dismantle Org Denote link to: \"DENOTEID Signature  Description\".
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
  "Append a title after the bare Denote ID.
With universal argument ARG, prepend the title before the ID."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (id (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (path (denote-get-path-by-id id))
         (file-type (denote-filetype-heuristics path))
         (title (denote-retrieve-title-or-filename path file-type))
         (signature (denote-retrieve-filename-signature path)))
    (unless title (user-error "Couldn't retrieve title"))
    (save-excursion
      (let ((prepend (equal arg '(4)))
            (sign (barezettel--format-signature signature)))
        (goto-char (if prepend (car bounds) (cdr bounds)))
        (if prepend
            (insert (if sign (string-join (list sign "  ")) "")
                    title " ")
          (insert (if sign (string-join (list " " sign "  ")) "  ")
                  title))))))

(defun barezettel-denote-link ()
  "Barezettel replacement for `denote-link'."
  (interactive)
  (save-excursion
    (let ((current-prefix-arg '(4)))
      (call-interactively 'denote-link))
    (if (org-in-regexp "\\[\\[denote:\\(.*?\\)\\].*\\]")
        (let ((id (match-string 1)))
          (replace-match (format "%s" id)))
      (user-error "Link format error"))
    (call-interactively #'barezettel-denote-append-title-at-point)))

(defun barezettel-denote-extract-region-and-link ()
  "Extract the region to a new note and leave a link to the new note."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((id nil)
         (buf (current-buffer))
         (beg (copy-marker (region-beginning)))
         (end (copy-marker (region-end) t))
         (advice (lambda (orig-fun &rest args)
                   (let ((path (apply orig-fun args)))
                     (setq id (denote-retrieve-filename-identifier path))
                     path))))
    (unwind-protect
        (progn
          (advice-add 'denote :around advice)
          (call-interactively #'denote-region))
      (advice-remove 'denote advice)
      (when (and id (buffer-live-p buf))
        (with-current-buffer buf
          (delete-region beg end) (goto-char beg) (insert id)))
      (set-marker beg nil)
      (set-marker end nil))))

;;;; Denote core functionality modifications and additions
;;;;; Allow bare Denote IDs to be registered as backlinks
(defun barezettel--denote-retrieve-xref-alist-for-backlinks (_orig-fn identifier)
  "Include raw occurrences of Denote ID IDENTIFIER in Denote backlinks.
ORIG-FN is the original backlink retrieval function."
  (denote-retrieve-xref-alist identifier))

(defun barezettel--setup-denote-backlinks (enable)
  "Toggle include Denote bare IDs in backlinks based on ENABLE.

Changes `denote-retrieve-xref-alist-for-backlinks' behavior so that it
includes every note that contains the ID anywhere in the note."
  (if enable
      (advice-add 'denote-retrieve-xref-alist-for-backlinks
                  :around #'barezettel--denote-retrieve-xref-alist-for-backlinks)
    (advice-remove 'denote-retrieve-xref-alist-for-backlinks
                   #'barezettel--denote-retrieve-xref-alist-for-backlinks)))

;;;;; Link as search
(defun barezettel--show-targets-in-denote-dired (files)
  "Show FILES in a `denote-dired' buffer."
  (let ((choices (mapcar #'file-relative-name files))
        (denote-sort-title-comparison-function (lambda (_a _b) nil))
        (dired-hide-details-mode t)
        (display-buffer-overriding-action
         '(display-buffer-use-some-window (inhibit-same-window . t))))
    (denote-sort-dired "" 'title nil nil choices)))

(defun barezettel--show-targets-in-completing-read (files)
  "Show FILES in a `completing-read'."
  (let* ((choices (mapcar (lambda (f)
                            (cons (file-relative-name f (car (denote-directories))) f))
                          files))
         (default-choice (car (car choices)))
         (selected (completing-read "Open the note: " choices nil t nil nil default-choice)))
    (when-let* ((path (cdr (assoc selected choices))))
      (if (equal arg '(4))
          (find-file-other-window path)
        (find-file path)))))

(defun barezettel-denote-show-targets-at-point (arg)
  "Show notes containing Denote ID at point.

Prompt for a target using `completing-read'.  The ID at point is the
default target.

When ARG is a single prefix argument, open the selected in another
window.

When ARG is a double prefix argument, display the targets in a separate
`denote-dired' buffer.

Also respect the variable `barezettel-show-targets-in-denote-dired'."
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
                 (all-files (delete-dups (cons current-file backlinks))))
            (cond
             ;; C-u C-u or user setting: open targets in a separate
             ;; denote-dired buffer
             ((or barezettel-show-targets-in-denote-dired
                  (equal arg '(16)))
              (barezettel--show-targets-in-denote-dired all-files))

             ;; Otherwise use completing-read
             (t (barezettel--show-targets-in-completing-read all-files)))))))))


;;;; Font lock rules for bare Denote IDs
(defvar-keymap barezettel-denote-id-keymap
  :doc "Keymap for barezettel ID buttons.")

(defvar barezettel--font-lock-keywords
  `((,barezettel-denote-regexp
     (0 `(face barezettel--denote-id-face
               keymap ,barezettel-denote-id-keymap
               help-echo barezettel--denote-id-help-echo)
        t)))
  "Keywords for `barezettel-mode' font-lock.")

(defun barezettel--denote-id-help-echo (_window _object position)
  "Return Denote filename for the Denote ID at POSITION."
  (save-excursion
    (goto-char position)
    (skip-chars-backward "0-9T")
    (when (looking-at barezettel-denote-regexp)
      (if-let* ((path (denote-get-path-by-id (match-string-no-properties 0)))
                (filename (file-name-nondirectory path)))
          (format "%s %s %s"
                  (or (denote-retrieve-filename-signature filename) "")
                  (or (denote-retrieve-filename-title filename) "")
                  (or (denote-extract-keywords-from-path filename) ""))
        "ID not detected in any Denote filename"))))

(defun barezettel--setup-font-lock (enable)
  "Enable or disable `barezettel-mode' font-lock rules based on ENABLE.

Any non-nil value for ENABLE enables behavior modifications, nil purges
the setup."
  (if enable
      (progn
        (add-to-list 'font-lock-extra-managed-props 'help-echo)
        (add-to-list 'font-lock-extra-managed-props 'keymap)

        (font-lock-add-keywords nil barezettel--font-lock-keywords 'prepend))
    (font-lock-remove-keywords nil barezettel--font-lock-keywords))
  (font-lock-flush))

;;;; Fill paragraph support for lines starting with Denote ID.
(defvar-keymap barezettel-mode--emulation-map
  :doc "Emulation keymap for `barezettel-mode'.
Only purpose for this is to remap a key used for `fill-paragraph' to
`barezettel-fill-paragraph' when the `barezettel-mode' is active."
  "<remap> <fill-paragraph>" #'barezettel-fill-paragraph)

(defvar-local barezettel--original-fill-paragraph-function nil
  "The original `fill-paragraph-function' before minor mode was enabled.")

(defvar-local barezettel--original-paragraph-start nil
  "The original variable `paragraph-start' before minor mode was enabled.")

(defvar-local barezettel--original-normal-auto-fill-function nil
  "The original variable `normal-auto-fill-function' before minor mode was enabled.")

(defun barezettel-fill-paragraph (&optional justify auto-fill)
  "Fill-paragraph that handles Denote ID lines too.

Prefix arg JUSTIFY non-nil means justify as well.  AUTO-FILL non-nil
means that this function can be used along with `auto-fill-mode'.

If the current line starts with a Denote ID followed by two spaces, fill
only the text section after the spaces while preserving the ID all the
way until spaces as an indentation for subsequent lines.  Adjacent lines
starting with with another Denote ID are treated as separate paragraphs
and are not merged.

Otherwise fall back to the major mode version or standard
`fill-paragraph'

When AUTO-FILL is non-nil fallback is `normal-auto-fill-function' that
was set before the minor mode was enabled."
  (interactive "P")
  (let ((cursor-orig (point)))
    (save-excursion
      (when (not (bolp))
        (backward-paragraph) (forward-char 1) (forward-line 0))

      (if (looking-at barezettel-denote-paragraph-start-regexp)
          (let* ((beg (match-end 0))
                 (id-length (- beg (match-beginning 0)))
                 (fill-prefix (make-string id-length ?\s))
                 (end (line-end-position)))
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp))
                          (not (looking-at "[ \t]*\$"))
                          (not (looking-at barezettel-denote-paragraph-start-regexp)))
                (setq end (line-end-position))
                (forward-line 1)))
            (fill-region-as-paragraph beg (1- end) justify))

        ;; Fallback fill-paragraph
        (if (and auto-fill auto-fill-function)
            ;; Are we called in auto-fill mode?
            (progn
              (goto-char cursor-orig)
              (if barezettel--original-normal-auto-fill-function
                  (funcall barezettel--original-normal-auto-fill-function)
                (funcall normal-auto-fill-function)))

          ;; No auto-fill-mode, just fill the paragraph
          (if (and barezettel--original-fill-paragraph-function
                   (commandp barezettel--original-fill-paragraph-function))
              (progn
                (goto-char cursor-orig)
                (funcall barezettel--original-fill-paragraph-function justify))
            (let ((fill-paragraph-function nil))
              (fill-paragraph justify))))))))

(defun barezettel-do-auto-fill ()
  "Custom auto-fill function for `barezettel-mode'."
  (when (> (current-column) fill-column)
    (save-excursion (barezettel-fill-paragraph nil t))))

(defun barezettel--setup-fill-paragraph (enable)
  "Setup or purge fill paragraph behavior based on ENABLE.

Any non-nil value for ENABLE enables behavior modifications, nil purges
the setup.

The setup affects both `paragraph-fill' and `auto-fill-mode'."

  (if enable
      (progn
        ;; Save originals
        (setq-local barezettel--original-fill-paragraph-function fill-paragraph-function)
        (setq-local barezettel--original-paragraph-start paragraph-start)
        (setq-local barezettel--original-normal-auto-fill-function normal-auto-fill-function)

        ;; Setup mode-specific values
        (setq-local fill-paragraph-function #'barezettel-fill-paragraph)
        (setq-local paragraph-start
                    (concat paragraph-start "\\|" "[ \t]*"
                            barezettel-denote-paragraph-start-regexp))
        (setq-local normal-auto-fill-function #'barezettel-do-auto-fill))

    ;; Restore originals
    (setq-local fill-paragraph-function barezettel--original-fill-paragraph-function)
    (setq-local paragraph-start barezettel--original-paragraph-start)
    (setq-local normal-auto-fill-function barezettel--original-normal-auto-fill-function)

    ;; Cleanup
    (setq-local barezettel--original-paragraph-start nil)))

;;;; Minor mode setup
(defvar barezettel--emulation-alist
  `((barezettel-mode . ,barezettel-mode--emulation-map)))

(unless (member 'barezettel--emulation-alist emulation-mode-map-alists)
  (add-to-list 'emulation-mode-map-alists 'barezettel--emulation-alist))

;;;###autoload
(define-minor-mode barezettel-mode
  "Toggles `barezettel-mode' minor mode.

Barezettel minor mode enables Denote interact with bare Denote IDs as if
they were links.  This is to avoid visual clutter and make using notes
outside Emacs more pleasant."
  :init-value nil
  :lighter " BZ"
  :keymap barezettel-mode-map

  (barezettel--setup-font-lock barezettel-mode)
  (barezettel--setup-fill-paragraph barezettel-mode)
  (barezettel--toggle-visual-hanging-indents barezettel-mode)
  (barezettel--setup-denote-backlinks barezettel-mode))

(defun barezettel--enable-if-denote-note ()
  "Enable `barezettel-mode' if the file is recognized as a Denote note."
  (when (and (buffer-file-name)
             (fboundp 'denote-file-is-note-p)
             (denote-file-is-note-p (buffer-file-name)))
    (barezettel-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-barezettel-mode barezettel-mode barezettel--enable-if-denote-note)

(provide 'barezettel)
