;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2021, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 16.6.20211130113540
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; License: GPL v2. Tell your friends to buy a copy.
;; Homepage: http://xahlee.info/emacs/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;; If you like this project, Buy Xah Emacs Tutorial http://xahlee.info/emacs/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;; HHH___________________________________________________________________

(defgroup xah-fly-keys nil
  "Ergonomic modal keybinding minor mode."
  :group 'keyboard)

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.
Version 2021-08-12"
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq $p1 (if (re-search-backward $blankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq $p2 (if (re-search-forward $blankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary,
else same as `xah-get-bounds-of-block'.
Version 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

;; HHH___________________________________________________________________
;; editing commands

(defun xah-copy-line-or-region ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region').

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_current_line.html'
Version 2019-10-30"
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if current-prefix-arg
        (progn
          (copy-region-as-kill (point-min) (point-max)))
      (if (region-active-p)
          (progn
            (copy-region-as-kill (region-beginning) (region-end)))
        (if (eq last-command this-command)
            (if (eobp)
                (progn )
              (progn
                (kill-append "\n" nil)
                (kill-append
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                 nil)
                (progn
                  (end-of-line)
                  (forward-char))))
          (if (eobp)
              (if (eq (char-before) 10 )
                  (progn )
                (progn
                  (copy-region-as-kill (line-beginning-position) (line-end-position))
                  (end-of-line)))
            (progn
              (copy-region-as-kill (line-beginning-position) (line-end-position))
              (end-of-line)
              (forward-char))))))))

(defun xah-cut-line-or-region ()
  "Cut current line or selection.
When `universal-argument' is called first, cut whole buffer
 (respects `narrow-to-region').

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (region-active-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-all-or-region ()
  "Copy buffer or selection content to `kill-ring'.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut buffer or selection content to `kill-ring'.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg,
paste that many times.

URL `http://xahlee.info/emacs/emacs/emacs_paste_or_paste_previous.html'
Version 2017-07-25 2020-09-08"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes (_ (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring spaces, then newlines, then spaces again, leaving one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* (($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t)
      (setq $eol-count (1+ $eol-count)))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if (> (- $p2 $p1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n")
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

(defun xah-fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call
toggles between short and long lines.
This commands calls `fill-region' to do its work.
Set `fill-column' for short line length.

URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2020-11-22 2021-08-13"
  (interactive)
  ;; This command symbol has a property “'longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($isLongline (if (eq last-command this-command) (get this-command 'longline-p) t))
         (deactivate-mark nil)
         $p1 $p2 )
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (if $isLongline
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'longline-p (not $isLongline))))

(defun xah-reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil 1)
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil 1)
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil 1)
        (replace-match " ")))))

(defun xah-reformat-to-multi-lines ( &optional Begin End MinLength)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version 2018-12-16 2021-07-06 2021-08-12"
  (interactive)
  (let ( $p1 $p2 $minlen )
    (setq $minlen (if MinLength MinLength (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

(defun xah-reformat-lines ( &optional Width)
  "Reformat current block or selection into short lines or 1 long line.
When called for the first time, change to one long line.
Second call change it to multiple short lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length
of line. By default, it is 70.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Created 2016 or before.
Version 2021-07-05 2021-08-13"
  (interactive)
  ;; This command symbol has a property 'is-long-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( $isLong $width $p1 $p2)
    (setq $width (if Width Width (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 70 )))
    (setq $isLong (if (eq last-command this-command) (get this-command 'is-long-p) nil))
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (progn
      (if current-prefix-arg
          (xah-reformat-to-multi-lines $p1 $p2 $width)
        (if $isLong
            (xah-reformat-to-multi-lines $p1 $p2 $width)
          (xah-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-long-p (not $isLong)))))

(defun xah-reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
HTML anchor links “<a…>…</a>” is also placed on a new line.
After this command is called, press space to repeat it.

URL `http://xahlee.info/emacs/emacs/elisp_reformat_to_sentence_lines.html'
Version 2020-12-02 2021-08-31"
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match " " ))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t) (replace-match " " ))
      (goto-char (point-min))
      (while (re-search-forward "\\. +\\([(0-9A-Za-z]+\\)" nil t) (replace-match ".\n\\1" ))
      (goto-char (point-min))
      (while (search-forward "<a " nil t) (replace-match "\n<a " ))
      (goto-char (point-min))
      (while (re-search-forward "<br */> *" nil t) (replace-match "<br />\n" ))
      (goto-char (point-max))
      (while (eq (char-before ) 32) (delete-char -1))))
  (re-search-forward "\n+" nil 1)
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "SPC") 'xah-reformat-to-sentence-lines ) $kmap)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char in current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version 2017-08-19 2021-08-12 2021-09-12 2021-11-28"
  (interactive)
  (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun xah-slash-to-backslash (&optional Begin End)
  "Replace slash by backslash on current line or region.
Version 2021-07-14 2021-09-12"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\"))))))

(defun xah-backslash-to-slash (&optional Begin End)
  "Replace backslash by slash on current line or region.
Version 2021-09-11"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "/"))))))

(defun xah-double-backslash (&optional Begin End)
  "Replace backslash by two backslash on current line or region.
Version 2021-11-09"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-single (&optional Begin End)
  "Replace double backslash by single backslash on current line or region.
Version 2021-11-09"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\"  nil t)
          (replace-match "\\\\"))))))

(defun xah-slash-to-double-backslash (&optional Begin End)
  "Replace slash by double backslash on current line or region.
Version 2021-07-14"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-slash (&optional Begin End)
  "Replace double backslash by slash on current line or region.
Version 2021-07-14"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\" nil t)
          (replace-match "/"))))))

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

(defun xah-quote-lines (Begin End QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

In lisp code, QuoteL QuoteR Sep are strings.

URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version 2020-06-26 2021-07-21 2021-08-15 2021-09-15"
  (interactive
   (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($brackets
          '(
            "\"double\""
            "'single'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            )) $bktChoice $sep $sepChoice $quoteL $quoteR)
     (setq $bktChoice (ido-completing-read "Quote to use:" $brackets))
     (setq $sepChoice (ido-completing-read "line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal $bktChoice "none")
       (setq $quoteL "" $quoteR ""))
      ((string-equal $bktChoice "other")
       (let (($x (read-string "Enter 2 chars, for begin/end quote:")))
         (setq $quoteL (substring-no-properties $x 0 1)
               $quoteR (substring-no-properties $x 1 2))))
      (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
               $quoteR (substring-no-properties $bktChoice -1))))
     (setq $sep
           (cond
            ((string-equal $sepChoice "none") "")
            ((string-equal $sepChoice "other") (read-string "Enter separator:"))
            (t $sepChoice)))
     (list $p1 $p2 $quoteL $quoteR $sep)))
  (let (($p1 Begin) ($p2 End) ($quoteL QuoteL) ($quoteR QuoteR) ($sep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert $quoteL)
            (end-of-line)
            (insert $quoteR $sep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))

(defun xah-escape-quotes (Begin End)
  "Add slash before double quote in current line or selection.
Double quote is codepoint 34.
See also: `xah-unescape-quotes'
URL `http://xahlee.info/emacs/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region Begin End)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" t t)))))

(defun xah-unescape-quotes (Begin End)
  "Replace  「\\\"」 by 「\"」 in current line or selection.
See also: `xah-escape-quotes'

URL `http://xahlee.info/emacs/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" t t)))))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to lowline _.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-10-04 2020-03-03"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        ;; (dired-next-line 1)
        (revert-buffer)
        )
    (user-error "Not in dired")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-10-04 2019-11-24"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "-" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
After this command is called, press space to repeat it.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.

URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version 2019-02-12 2021-08-20"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let* ($p1
         $p2
         ($charArray ["-" "_" " "])
         ($n (length $charArray))
         ($regionWasActive-p (region-active-p))
         ($nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
         ($changeTo (elt $charArray $nowState)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point))
          (set-mark $p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward (elt $charArray (% (+ $nowState 2) $n)) (point-max) 1)
          (replace-match $changeTo t t))))
    (when (or (string-equal $changeTo " ") $regionWasActive-p)
      (goto-char $p2)
      (set-mark $p1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $n)))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "SPC") 'xah-cycle-hyphen-lowline-space) $kmap)))

(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

(defun xah-delete-current-text-block ()
  "Delete the current text block plus blank lines, or selection, and copy to `kill-ring'.

URL `http://xahlee.info/emacs/emacs/emacs_delete_block.html'
Version 2017-07-09 2021-08-14"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq $p1 (goto-char (match-end 0)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n+" nil 1)
        (setq $p2 (point))))
    (kill-region $p1 $p2)))

(defun xah-clear-register-1 ()
  "Clear register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Cleared register 1.")))

(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2017-01-23"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-append-to-register-1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_append.html'
Version 2015-12-08 2020-09-08"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (append-to-register ?1 $p1 $p2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat 'identity (extract-rectangle Begin End) "\n")))

;; HHH___________________________________________________________________
;; insertion commands

(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first.

URL `http://xahlee.info/emacs/emacs/elisp_insert-date-time.html'
version 2020-09-07 2021-11-07"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 20180412224611"
                  "2 → 2018-04-12_224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → 2018-04-12 Thursday"
                  "6 → Thursday, April 12, 2018"
                  "7 → Thu, Apr 12, 2018"
                  "8 → April 12, 2018"
                  "9 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "1 → 20180412224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 2)
       ;; "2 → 2018-04-12_224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y-%m-%d_%T")))
      ((= $style 3)
       ;; "3 → 2018-04-12T22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 4)
       ;; "4 → 2018-04-12 22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 5)
       ;; "5 → 2018-04-12 Thursday"
       (format-time-string "%Y-%m-%d %A"))
      ((= $style 6)
       ;; "6 → Thursday, April 12, 2018"
       (format-time-string "%A, %B %d, %Y"))
      ((= $style 7)
       ;; "7 → Thu, Apr 12, 2018"
       (format-time-string "%a, %b %d, %Y"))
      ((= $style 8)
       ;; "8 → April 12, 2018"
       (format-time-string "%B %d, %Y"))
      ((= $style 9)
       ;; "9 → Apr 12, 2018"
       (format-time-string "%b %d, %Y"))
      (t
       (format-time-string "%Y-%m-%d"))))))

(defun xah-insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 LBracket and RBracket are strings. WrapMethod must be either 'line or 'block. 'block means between empty lines.

• if there is a region, add brackets around region.
• If WrapMethod is 'line, wrap around line.
• If WrapMethod is 'block, wrap around block.
• if cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html'
Version 2017-01-17 2021-08-12"
  (if (region-active-p)
      (progn
        (let ( ($p1 (region-beginning)) ($p2 (region-end)))
          (goto-char $p2) (insert RBracket)
          (goto-char $p1) (insert LBracket)
          (goto-char (+ $p2 2))))
    (let ($p1 $p2)
      (cond
       ((eq WrapMethod 'line)
        (setq $p1 (line-beginning-position) $p2 (line-end-position))
        (goto-char $p2)
        (insert RBracket)
        (goto-char $p1)
        (insert LBracket)
        (goto-char (+ $p2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
          (goto-char $p2)
          (insert RBracket)
          (goto-char $p1)
          (insert LBracket)
          (goto-char (+ $p2 (length LBracket)))))
       ( ;  do line. line must contain space
        (and
         (eq (point) (line-beginning-position))
         ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (not (eq (line-beginning-position) (line-end-position))))
        (insert LBracket )
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; cursor is at end of word or buffer. i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (or
               (string-equal major-mode "xah-elisp-mode")
               (string-equal major-mode "emacs-lisp-mode")
               (string-equal major-mode "lisp-mode")
               (string-equal major-mode "lisp-interaction-mode")
               (string-equal major-mode "common-lisp-mode")
               (string-equal major-mode "clojure-mode")
               (string-equal major-mode "xah-clojure-mode")
               (string-equal major-mode "scheme-mode"))))
        (progn
          (setq $p1 (point) $p2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket )))
       (t (progn
            ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq $p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq $p2 (point))
            (goto-char $p2)
            (insert RBracket)
            (goto-char $p1)
            (insert LBracket)
            (goto-char (+ $p2 (length LBracket)))))))))

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )

(defun xah-insert-double-curly-quote () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket () (interactive) (xah-insert-bracket-pair "「" "」" ) )
(defun xah-insert-white-corner-bracket () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket () (interactive) (xah-insert-bracket-pair "〔" "〕" ) )

(defun xah-insert-hyphen ()
  "Insert a HYPHEN-MINUS character."
  (interactive)
  (insert "-"))

(defun xah-insert-low-line ()
  "Insert a LOW LINE character."
  (interactive)
  (insert "_"))

(defun xah-insert-string-assignment ()
  "Insert =\"\""
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\n\u000c\n"))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.

URL `http://xahlee.info/emacs/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

(defun xah-insert-column-az ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode.

URL `http://xahlee.info/emacs/emacs/emacs_insert-alphabets.html'
Version 2019-03-07"
  (interactive)
  (let (
        ($startChar (string-to-char (read-string "Start char: " "a")))
        ($howmany (string-to-number (read-string "How many: " "26")))
        ($colpos (- (point) (line-beginning-position))))
    (dotimes ($i $howmany )
      (progn
        (insert-char (+ $i $startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char $colpos)))))

(defvar xah-unicode-list nil "Associative list of Unicode symbols. First element is a Unicode character, second element is a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ;; format: (str . nameOrFastKey)
        ("_" . "underscore" )
        ("•" . ".bullet" )
        ("→" . "tn")
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "...ellipsis" )
        (" " . "nbsp" )
        ("、" . "," )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-emdash" )
        ("＆" . "7fullwidthAmpersand" )
        ("↓" . "downArrow")
        ("←" . "leftArrow")
        ("↑" . "upArrow")
        ("👍" . "thumbUp")
        ("〚〛" . "whiteSquareBracket")
        ) )

(defun xah-insert-unicode ()
  "Insert a unicode from a custom list `xah-unicode-list'.
Version 2021-01-05"
  (interactive)
  (let (
        ($str
         (ido-completing-read
          "Insert:" (mapcar
                     (lambda (x)
                       (format "%s %s" (car x) (cdr x))) xah-unicode-list))))
    (insert (car (split-string $str " " t)))))

;; HHH___________________________________________________________________
;; text selection

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version 2017-11-01 2021-03-19"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let (($p1 (point)))
                (end-of-visual-line 1)
                (when (eq $p1 (point))
                  (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (set-mark (point))
               (end-of-visual-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position))))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• if cursor is on a any type of bracket (including parenthesis,
quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior
is still experimental. But when cursor is on a any type of
bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version 2020-02-04"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))

;; HHH___________________________________________________________________
;; misc

(defun xah-user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user buffer.
This function is used by buffer switching command and close buffer
command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(if (version<= emacs-version "26.0.50")
    (defalias 'xah-display-line-numbers-mode #'linum-mode)
  (defalias 'xah-display-line-numbers-mode #'global-display-line-numbers-mode))

(defvar xah-fly-M-x-command nil "Command to call for emacs
`execute-extended-command' replacement, used by `xah-fly-M-x'.
 Value should be a lisp symbol.")

(setq xah-fly-M-x-command nil)

(defun xah-fly-M-x ()
  "Calls `execute-extended-command' or an alternative.
If `xah-fly-M-x-command' is non-nil, call it, else call one of the following,
in order: `smex', `helm-M-x', `counsel-M-x', `execute-extended-command'.
Version 2020-04-09 2021-02-24"
  (interactive)
  (command-execute
   (cond
    ((and (boundp 'xah-fly-M-x-command) xah-fly-M-x-command) xah-fly-M-x-command )
    ((fboundp 'smex) 'smex)
    ((fboundp 'helm-M-x) 'helm-M-x)
    ((fboundp 'counsel-M-x) 'counsel-M-x)
    (t 'execute-extended-command))
   nil
   nil
   :special))

;; HHH___________________________________________________________________

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2021-08-27"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) ? ) (delete-char -1))))))

;; HHH___________________________________________________________________
;; G3R functions

(defun g3r-toggle-maximize-buffer ()
  "Toggles fullscreen/zoom for currently selected buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; HHH___________________________________________________________________
;; key maps for conversion

(defun xah-fly--key-char (Charstr)
  "Return the corresponding char Charstr according to
`xah-fly--current-layout-kmap'.
Charstr must be a string of single char. If more than 1 char,
return it unchanged.
Version 2020-04-18"
  (interactive)
  (if (> (length Charstr) 1)
      Charstr
    (let (($result (assoc Charstr nil)))
      (if $result (cdr $result) Charstr ))))

(defmacro xah-fly--define-keys (KeymapName KeyCmdAlist &optional DirectQ)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
The key is remapped from Dvorak to the current keyboard layout
by `xah-fly--key-char'.
If DirectQ is t, do not remap key to current keyboard layout.
Example usage:
;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-fly-dot-keymap)
;;  '(
;;    (\"h\" . highlight-symbol-at-point)
;;    (\".\" . isearch-forward-symbol-at-point)
;;    (\"1\" . hi-lock-find-patterns)
;;    (\"w\" . isearch-forward-word)))
Version 2020-04-18"
  (let (($keymapName (make-symbol "keymap-name")))
    `(let ((,$keymapName , KeymapName))
       ,@(mapcar
          (lambda ($pair)
            `(define-key
               ,$keymapName
               (kbd (,(if DirectQ #'identity #'xah-fly--key-char) ,(car $pair)))
               ,(list 'quote (cdr $pair))))
          (cadr KeyCmdAlist)))))

;; HHH___________________________________________________________________
;; keymaps

(defvar xah-fly-key-map (make-sparse-keymap)
  "Backward-compatibility map for `xah-fly-keys' minor mode.

Points to `xah-fly-insert-map' when `xah-fly-insert-state-p' is non-nil,
and points to `xah-fly-command-map' otherwise (which see).")
(make-obsolete-variable
 'xah-fly-key-map
 "Put bindings for command mode in `xah-fly-command-map', bindings for
insert mode in `xah-fly-insert-map' and bindings that are common to both
command and insert modes in `xah-fly-shared-map'."
 "2020-04-16")

(defvar xah-fly-shared-map (make-sparse-keymap)
  "Parent keymap of `xah-fly-command-map' and `xah-fly-insert-map'.

Define keys that are available in both command and insert modes here, like
`xah-fly-mode-toggle'")

;; (cons 'keymap xah-fly-shared-map) makes a new keymap with `xah-fly-shared-map' as its parent. See info node (elisp)Inheritance and Keymaps.
(defvar xah-fly-command-map (cons 'keymap xah-fly-shared-map)
  "Keymap that takes precedence over all other keymaps in command mode.

Inherits bindings from `xah-fly-shared-map'. In command mode, if no binding
is found in this map `xah-fly-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `xah-fly-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `xah-fly-shared-map' can be disabled
by this map.

Effectively, this map takes precedence over all others when command mode
is enabled.")

(defvar xah-fly-insert-map (cons 'keymap xah-fly-shared-map)
  "Keymap for bindings that will be checked in insert mode. Active whenever
`xah-fly-keys' is non-nil.

Inherits bindings from `xah-fly-shared-map'. In insert mode, if no binding
is found in this map `xah-fly-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `xah-fly-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `xah-fly-shared-map' can be disabled
by this map.

Keep in mind that this acts like a normal global minor mode map, so other
minor modes loaded later may override bindings in this map.")

(defvar xah-fly--deactivate-command-mode-func nil)

;; HHH___________________________________________________________________
;; setting keys

(xah-fly--define-keys
 xah-fly-command-map
 '(
   ("~" . nil)
   (":" . nil)

   ("SPC" . xah-fly-leader-key-map)
   ("DEL" . xah-fly-leader-key-map)
   ("'" . xah-reformat-lines)
   ("," . xah-shrink-whitespaces)
   ("." . backward-kill-word)
   (";" . comment-line)
   ("/" . hippie-expand)
   ("\\" . nil)
   ("=" . nil)

   ("1" . xah-extend-selection)
   ("2" . xah-select-line)
   ("3" . delete-other-windows)
   ("4" . split-window-below)

   ("a" . xah-fly-M-x)
   ("b" . isearch-forward)
   ("c" . previous-line)
   ("d" . back-to-indentation)
   ("e" . delete-backward-char)
   ("f" . undo)
   ("g" . backward-word)
   ("h" . backward-char)
   ("i" . delete-forward-char)
   ("j" . xah-copy-line-or-region)
   ("k" . xah-paste-or-paste-previous)
   ("l" . recenter-top-bottom)
   ("m" . backward-list)
   ("n" . forward-char)
   ("o" . open-line)
   ("p" . kill-word)
   ("q" . xah-cut-line-or-region)
   ("r" . forward-word)
   ("s" . end-of-line)
   ("t" . next-line)
   ("u" . xah-fly-insert-mode-activate)
   ("v" . forward-list)
   ("w" . other-window)
   ("y" . set-mark-command)))

;; HHH___________________________________________________________________
;; set control meta, etc keys

(xah-fly--define-keys
 xah-fly-shared-map
 '(("<home>" . xah-fly-command-mode-activate))
 :direct)

;; HHH___________________________________________________________________
;; commands related to highlight
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-dot-keymap)
 ;; 2019-02-22 experiment. this is now empty. so you can use this key space for all major mode custom keys or personal keys. These highlight command isn't used much in my experience
 '(
   ))

;; HHH___________________________________________________________________

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-c-keymap)
 '(
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("l" . bookmark-set)
   ("r" . bookmark-jump)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-e-keymap)
 '(
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-h-keymap)
 '(
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("h" . describe-function)
   ("i" . info)
   ("j" . man)
   ("k" . describe-key)
   ("l" . view-lossage)
   ("m" . describe-mode)
   ("n" . describe-variable)
   ("r" . apropos-variable)
   ("u" . elisp-index-search)
   ))

(xah-fly--define-keys
 ;; commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-fly-n-keymap)
 '(
   ("SPC" . whitespace-mode)
   (","   . abbrev-mode)
   ("."   . toggle-frame-fullscreen)
   ("2"   . global-hl-line-mode)
   ("4"   . global-display-line-numbers-mode)
   ("e"   . eshell)
   ("f"   . g3r-toggle-maximize-buffer)
   ("o"   . variable-pitch-mode)
   ("u"   . shell)
   ("v"   . visual-line-mode)
   ("y"   . toggle-truncate-lines)
   ))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-r-keymap)
 '(("SPC" . rectangle-mark-mode)
   (","   . apply-macro-to-region-lines)
   ("."   . kmacro-start-macro)
   ("c"   . replace-rectangle)
   ("d"   . delete-rectangle)
   ("e"   . call-last-kbd-macro)
   ("g"   . kill-rectangle)
   ("l"   . clear-rectangle)
   ("n"   . rectangle-number-lines)
   ("o"   . open-rectangle)
   ("p"   . kmacro-end-macro)
   ("r"   . yank-rectangle)
   ("y"   . delete-whitespace-rectangle)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-t-keymap)
 '(
   ("SPC" . xah-clean-whitespace)
   ("." . sort-lines)

   ("d" . mark-defun)
   ("e" . list-matching-lines)
))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-w-keymap)
 '(
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-comma-keymap)
 '(
   ("t" . xref-find-definitions)
   ("n" . xref-pop-marker-stack)))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-leader-key-map)
 '(
   ("SPC" . xah-fly-insert-mode-activate)
   ("TAB" . xah-fly--tab-key-map)
   ("." . xah-fly-dot-keymap)
   ("'" . xah-fill-or-unfill)
   ("," . xah-fly-comma-keymap)

   ("3" . delete-window)
   ("4" . split-window-right)
   ("5" . balance-windows)

   ("a" . mark-whole-buffer)
   ("b" . end-of-buffer)
   ("c" . xah-fly-c-keymap)
   ("d" . beginning-of-buffer)
   ("e" . xah-fly-e-keymap)
   ("h" . xah-fly-h-keymap)
   ("i" . kill-line)
   ("l" . recenter-top-bottom)
   ("m" . dired-jump)
   ("n" . xah-fly-n-keymap)
   ("o" . exchange-point-and-mark)
   ("p" . query-replace)
   ("r" . xah-fly-r-keymap)
   ("s" . save-buffer)
   ("t" . xah-fly-t-keymap)
   ("u" . switch-to-buffer)
   ("w" . xah-fly-w-keymap)
   ))

;; HHH___________________________________________________________________
;; Movement key integrations with built-in Emacs packages

(xah-fly--define-keys
 indent-rigidly-map
 '(("h" . indent-rigidly-left)
   ("n" . indent-rigidly-right)))

;; HHH___________________________________________________________________

(defvar xah-fly-insert-state-p t "non-nil means insertion mode is on.")

(defun xah-fly--update-key-map ()
  (setq xah-fly-key-map (if xah-fly-insert-state-p
                            xah-fly-insert-map
                          xah-fly-command-map)))

(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version 2020-04-28"
  (interactive)
  (setq xah-fly-insert-state-p nil)
  (xah-fly--update-key-map)
  (setq xah-fly--deactivate-command-mode-func
        (set-transient-map xah-fly-command-map (lambda () t)))
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (set-face-background 'cursor "firebrick1")
  (setq mode-line-front-space (concat (propertize " NORMAL " 'face '(:inherit (fixed-pitch) :weight bold :foreground "grey90" :background "firebrick4")) " "))
  (force-mode-line-update))

(defun xah-fly-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq xah-fly-insert-state-p t)
  (xah-fly--update-key-map)
  (funcall xah-fly--deactivate-command-mode-func)
  (unless no-indication
    (modify-all-frames-parameters '((cursor-type . bar)))
    (set-face-background 'cursor "chartreuse")
    (setq mode-line-front-space (concat (propertize " INSERT " 'face '(:inherit (fixed-pitch) :weight bold :foreground "grey90" :background "#105020")) " ")))
  (force-mode-line-update))

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode and run `xah-fly-command-mode-activate-hook'
Version 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-command-mode-activate-no-hook ()
  "Activate command mode without running `xah-fly-command-mode-activate-hook'
Version 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode.
Version 2017-07-07"
  (interactive)
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

;; HHH___________________________________________________________________

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout.

URL `http://xahlee.info/emacs/misc/ergoemacs_vi_mode.html'"
  :group 'xah-fly-keys
  :global t
  :lighter " XFK"
  :keymap xah-fly-insert-map
  (delete-selection-mode 1)
  (setq shift-select-mode nil)

  (if xah-fly-keys
      ;; Construction:
      (progn
        (add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
        (add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
        (add-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
        (when (and (keymapp xah-fly-key-map)
                   (not (memq xah-fly-key-map (list xah-fly-command-map
                                                    xah-fly-insert-map))))
          (set-keymap-parent xah-fly-key-map xah-fly-shared-map)
          (setq xah-fly-shared-map xah-fly-key-map))
        (xah-fly-command-mode-activate))
    (progn
      ;; Teardown:
      (remove-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
      (remove-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
      (remove-hook 'eshell-mode-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
      (xah-fly-insert-mode-init :no-indication)
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))

      ;;
      )))

(provide 'xah-fly-keys)

;;; xah-fly-keys.el ends here
