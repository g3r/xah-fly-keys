;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2021, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 15.21.20211009110616
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; License: GPL v3
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

;; It is a modal mode like vi, but key choices are based on statistics of command call frequency.

;; --------------------------------------------------
;; MANUAL INSTALL

;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty") ; required

;; possible layout values:

;; dvorak
;; qwerty

;; (xah-fly-keys 1)

;; --------------------------------------------------
;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press 【<home>】 or 【F8】 or 【Alt+Space】 or 【menu】)

;; xah-fly-insert-mode-activate  (when in command mode, press qwerty letter key f. (Dvorak key u))

;; When in command mode:
;; 【f】 (or Dvorak 【u】) activates insertion mode.
;; 【Space】 is a leader key. For example, 【SPACE r】 (Dvorak 【SPACE p】) calls query-replace. Press 【SPACE C-h】 to see the full list.
;; 【Space Space】 also activates insertion mode.
;; 【Space Enter】 calls execute-extended-command or alternative.
;; 【a】 calls execute-extended-command or alternative.

;; The leader key sequence basically replace ALL emacs commands that starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel.
;; C-q for quoted-insert.
;; C-h for getting a list of keys following a prefix/leader key.

;; Leader key

;; You NEVER need to press Ctrl+x

;; Any emacs command that has a keybinding starting with C-x, has also a key sequence binding in xah-fly-keys. For example,
;; 【C-x b】 switch-to-buffer is 【SPACE f】 (Dvorak 【SPACE u】)
;; 【C-x C-f】 find-file is 【SPACE i e】 (Dvorak 【SPACE c .】)
;; 【C-x n n】 narrow-to-region is 【SPACE l l】 (Dvorak 【SPACE n n】)
;; The first key we call it leader key. In the above examples, the SPACE is the leader key.

;; When in command mode, the 【SPACE】 is a leader key.

;; the following standard keys with Control are supported:

 ;; 【Ctrl+tab】 'xah-next-user-buffer
 ;; 【Ctrl+shift+tab】 'xah-previous-user-buffer
 ;; 【Ctrl+v】 paste
 ;; 【Ctrl+w】 close
 ;; 【Ctrl+z】 undo
 ;; 【Ctrl+n】 new
 ;; 【Ctrl+o】 open
 ;; 【Ctrl+s】 save
 ;; 【Ctrl+shift+s】 save as
 ;; 【Ctrl+shift+t】 open last closed
 ;; 【Ctrl++】 'text-scale-increase
 ;; 【Ctrl+-】 'text-scale-decrease

;; I highly recommend setting 【capslock】 to send 【Home】. So that it acts as `xah-fly-command-mode-activate'.
;; see
;; How to Make the CapsLock Key do Home Key
;; http://ergoemacs.org/misc/capslock_do_home_key.html

;; If you have a bug, post on github.

;; For detail about design and other info, see home page at
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;; HHH___________________________________________________________________
;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs

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
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.
Version 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

;; HHH___________________________________________________________________
;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.

URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous block.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, beginning of line means visual line.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04 2021-03-16"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil 1)
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (if visual-line-mode
          (beginning-of-visual-line)
        (progn
          (back-to-indentation)
          (when (eq $p (point))
            (beginning-of-line)))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next block.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04 2021-03-16"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil 1 )
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar xah-left-brackets '( "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")
  "List of left bracket chars.")

(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '( ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")

(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

;; HHH___________________________________________________________________
;; editing commands

(defun xah-copy-line-or-region ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
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
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
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

URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
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

URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
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

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
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

(defun xah-delete-backward-char-or-bracket-text ()
  "Delete backward 1 character or delete quote/bracket pair and inner text.
If the char to the left of cursor is a matching pair, delete it and inner text, push the deleted text to `kill-ring'.

What char is considered bracket or quote is determined by current syntax table.

If `universal-argument' is called first, do not delete inner text.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (xah-delete-backward-bracket-pair)
        (xah-delete-backward-bracket-text)))
     ((looking-back "\\s(" 1)
      (progn
        (backward-char)
        (forward-sexp)
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char)
            (xah-delete-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.

This command assumes the left of cursor is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-09-21"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.
After call, mark is set at the matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the left of point is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))

(defun xah-delete-forward-bracket-pairs ( &optional DeleteInnerTextQ)
  "Delete the matching brackets/quotes to the right of cursor.
If DeleteInnerTextQ is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if DeleteInnerTextQ
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2020-06-26"
  (interactive)
  (let ( (deactivate-mark nil) $p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
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
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2019-06-13"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (xah-fly-delete-spaces)
        (progn (xah-fly-delete-spaces)
               (insert " ")))
      )
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
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun xah-fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column' for short line length.

URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
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

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
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

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
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
When called for the first time, change to one long line. Second call change it to multiple short lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length of line. By default, it is 70.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
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

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
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

(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
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

URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
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

;; HHH___________________________________________________________________
;; insertion commands

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

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
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

(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )

(defun xah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\n\u000c\n"))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.

URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

;; HHH___________________________________________________________________
;; text selection

(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
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
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
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

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
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
“user buffer” is determined by `xah-user-buffer-q'.

URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.

URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun xah-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but use `ido-mode' interface, and ignore cursor position in bookmark.

URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($thisBookmark (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $thisBookmark))))

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17 2021-02-24 2021-08-14 2021-09-19"
  (interactive)
  (let* (($input
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let (($p0 (point)) $p1 $p2
                  ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path (replace-regexp-in-string "^/C:/" "/" (replace-regexp-in-string "^file://" "" (replace-regexp-in-string ":\\'" "" $input)))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x)
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path)
            (let (($fpath (substring $path 0 (match-beginning 0)))
                  ($fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (($fpath (match-string-no-properties 1 $path))
                    ($lineNum (string-to-number (match-string-no-properties 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $lineNum)))
                  (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $path))
                  (find-file $path))))))))))

(if (version<= emacs-version "26.0.50")
    (defalias 'xah-display-line-numbers-mode #'linum-mode)
  (defalias 'xah-display-line-numbers-mode #'global-display-line-numbers-mode))

(defvar xah-fly-M-x-command nil "Command to call for emacs `execute-extended-command' replacement, used by `xah-fly-M-x'. Value should be a lisp symbol.")

(setq xah-fly-M-x-command nil)

(defun xah-fly-M-x ()
  "Calls `execute-extended-command' or an alternative.
If `xah-fly-M-x-command' is non-nil, call it, else call one of the following, in order: `smex', `helm-M-x', `counsel-M-x', `execute-extended-command'.
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

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1, in whole buffer or selection.
Respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil 1)
            (replace-match "\n\n")))))))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
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

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

(declare-function w32-shell-execute "w32fns.c" (operation document &optional parameters show-flag)) ; (w32-shell-execute "open" default-directory)

(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Windows Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-20 2021-01-31"
  (interactive)
  (let (($path (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files ))
                       default-directory
                     (car (dired-get-marked-files )))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let ( ($cmd (format "Explorer /select,%s"  (shell-quote-argument (replace-regexp-in-string "/" "\\" $path t t )))))
      ;;   (shell-command $cmd))
      )
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (let (
            (process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-02-13 2021-01-18"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ))))
    (message "path is %s" $path)
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument $path))))
     ((string-equal system-type "windows-nt")
      ;; 2021-01-18 problem: if gnu findutils is installed, it installs a code.exe program, same name as vscode's executable. and usually in path before vscode.
;; vs code is usually at home dir
;; "C:\Users\joe\AppData\Local\Programs\Microsoft VS Code\bin\code.cmd"
      ;; the following is attemp to work around
      ;; (shell-command
      ;;  (format
      ;;   "PowerShell -Command Invoke-Expression \"%s\\%s\" %s"
      ;;   (getenv "HOMEPATH")
      ;;   "AppData\\Local\\Programs\\Microsoft VS Code\\Code.exe"
      ;;   (shell-quote-argument $path)))
      ;; (shell-command (concat "PowerShell -Command Start-Process Code.exe -filepath " (shell-quote-argument $path)))
      (shell-command (format "Code %s" (shell-quote-argument $path)))
      ;;
      )
     ((string-equal system-type "gnu/linux")
      (shell-command (format "code %s" (shell-quote-argument $path)))))))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-07-21"
  (interactive)
  (let ($fileList $doIt )
    (setq $fileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
    (setq $doIt (if (<= (length $fileList) 5) t (y-or-n-p "Open more than 5 files? ")))
    (when $doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $fileList))
       ((string-equal system-type "darwin")
        (mapc (lambda ($fpath) (shell-command (concat "open " (shell-quote-argument $fpath)))) $fileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $fileList))
       ((string-equal system-type "berkeley-unix")
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $fileList))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, it starts cross-platform PowerShell pwsh. You need to have it installed.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-21 2021-07-21"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((process-connection-type nil)
          ($cmdstr
           (format "pwsh -Command Start-Process pwsh -WorkingDirectory %s" (shell-quote-argument default-directory))))
      ;; (start-process "" nil "powershell" "Start-Process" "powershell"  "-WorkingDirectory" default-directory)
      (shell-command $cmdstr)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory )))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((string-equal system-type "berkeley-unix")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame.
Version 2017-01-27"
  (interactive)
  (if current-prefix-arg
      (other-frame 1)
    (if (one-window-p)
        (other-frame 1)
      (other-window 1))))

(defun xah-unsplit-window-or-next-frame ()
  "Unsplit window. If current frame has only one window, switch to next frame.
Version 2017-01-29"
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (delete-other-windows)))

;; HHH___________________________________________________________________
;; key maps for conversion

(defvar xah--dvorak-to-dvorak-kmap
  '()
  "A alist, dvorak to dvorak.")

(defvar xah--dvorak-to-qwerty-kmap
  '(("." . "e")
    ("," . "w")
    ("'" . "q")
    (";" . "z")
    ("/" . "[")
    ("[" . "-")
    ("]" . "=")
    ("=" . "]")
    ("-" . "'")
    ("a" . "a")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . ";")
    ("t" . "k")
    ("u" . "f")
    ("v" . ".")
    ("w" . ",")
    ("x" . "b")
    ("y" . "t")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding QWERTY. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defcustom xah-fly-key-current-layout nil
  "The current keyboard layout. Use `xah-fly-keys-set-layout' to set the layout.
If the value is nil, it is automatically set to \"dvorak\"."
  :type '(choice
          (const :tag "Dvorak" dvorak)
          (const :tag "QWERTY" qwerty))
  :group 'xah-fly-keys
  :set (lambda (Layout-var New-layout)
         ;; Only reload xah-fly-keys if it was already loaded and the new layout is different:
         (if (and (featurep 'xah-fly-keys)
                  (not (eq New-layout (symbol-value Layout-var))))
             (progn
               (set Layout-var New-layout)
               (load "xah-fly-keys"))
           (set Layout-var New-layout))))
(if xah-fly-key-current-layout nil (setq xah-fly-key-current-layout 'qwerty))

(defvar xah-fly--current-layout-kmap nil
  "The current keyboard layout key map. Value is a alist. e.g. the value of `xah--dvorak-to-qwerty-kmap'.
Value is automatically set from value of `xah-fly-key-current-layout'. Do not manually set this variable. Version 2019-02-12."
  )
(setq xah-fly--current-layout-kmap
      (symbol-value
       (intern
        (concat "xah--dvorak-to-"
                (if (symbolp xah-fly-key-current-layout)
                    (symbol-name xah-fly-key-current-layout)
                  xah-fly-key-current-layout)
                "-kmap"))))

(defun xah-fly--key-char (Charstr)
  "Return the corresponding char Charstr according to `xah-fly--current-layout-kmap'.
Charstr must be a string of single char. If more than 1 char, return it unchanged.
Version 2020-04-18"
  (interactive)
  (if (> (length Charstr) 1)
      Charstr
    (let (($result (assoc Charstr xah-fly--current-layout-kmap)))
      (if $result (cdr $result) Charstr ))))

(defmacro xah-fly--define-keys (Keymap-name Key-cmd-alist &optional Direct-q)
  "Map `define-key' over a alist Key-cmd-alist, with key layout remap.
The key is remapped from Dvorak to the current keyboard layout
by `xah-fly--key-char'.
If Direct-q is t, do not remap key to current keyboard layout.
Example usage:
;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-fly-dot-keymap)
;;  '(
;;    (\"h\" . highlight-symbol-at-point)
;;    (\".\" . isearch-forward-symbol-at-point)
;;    (\"1\" . hi-lock-find-patterns)
;;    (\"w\" . isearch-forward-word)))
Version 2020-04-18"
  (let (($keymap-name (make-symbol "keymap-name")))
    `(let ((,$keymap-name , Keymap-name))
       ,@(mapcar
          (lambda ($pair)
            `(define-key
               ,$keymap-name
               (kbd (,(if Direct-q #'identity #'xah-fly--key-char) ,(car $pair)))
               ,(list 'quote (cdr $pair))))
          (cadr Key-cmd-alist)))))

;; HHH___________________________________________________________________
;; keymaps

(defvar xah-fly-key-map (make-sparse-keymap)
  "Backward-compatibility map for `xah-fly-keys' minor mode.

Points to `xah-fly-insert-map' when `xah-fly-insert-state-q' is non-nil,
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
   (";" . xah-comment-dwim)
   ("/" . hippie-expand)
   ("\\" . nil)
   ("=" . nil)
   ("[" . xah-backward-punct )
   ("]" . xah-forward-punct)
   ("`" . other-frame)

   ("1" . xah-extend-selection)
   ("2" . xah-select-line)
   ("3" . delete-other-windows)
   ("4" . split-window-below)
   ("5" . delete-char)
   ("6" . xah-select-block)
   ("7" . xah-select-line)
   ("8" . xah-extend-selection)
   ("0" . xah-pop-local-mark-ring)

   ("a" . xah-fly-M-x)
   ("b" . isearch-forward)
   ("c" . previous-line)
   ("d" . xah-beginning-of-line-or-block)
   ("e" . xah-delete-backward-char-or-bracket-text)
   ("f" . undo)
   ("g" . backward-word)
   ("h" . backward-char)
   ("i" . xah-delete-current-text-block)
   ("j" . xah-copy-line-or-region)
   ("k" . xah-paste-or-paste-previous)
   ("m" . xah-backward-left-bracket)
   ("n" . forward-char)
   ("o" . open-line)
   ("p" . kill-word)
   ("q" . xah-cut-line-or-region)
   ("r" . forward-word)
   ("s" . xah-end-of-line-or-block)
   ("t" . next-line)
   ("u" . xah-fly-insert-mode-activate)
   ("v" . xah-forward-right-bracket)
   ("w" . xah-next-window-or-frame)
   ("x" . xah-toggle-letter-case)
   ("y" . set-mark-command)
   ("z" . xah-goto-matching-bracket)))

;; HHH___________________________________________________________________
;; set control meta, etc keys

(xah-fly--define-keys
 xah-fly-shared-map
 '(("<home>" . xah-fly-command-mode-activate)
   ("<menu>" . xah-fly-command-mode-activate))
   ;; ("<f8>" . xah-fly-command-mode-activate-no-hook))
 :direct)

;; HHH___________________________________________________________________
;; commands related to highlight
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-dot-keymap)
 ;; 2019-02-22 experiment. this is now empty. so you can use this key space for all major mode custom keys or personal keys. These highlight command isn't used much in my experience
 '(
   ("." . highlight-symbol-at-point)
   ("g" . unhighlight-regexp)
   ("c" . highlight-lines-matching-regexp)
   ("h" . highlight-regexp)
   ("t" . highlight-phrase)
   ;; ("p" . isearch-forward-symbol-at-point)
   ;; ("c" . isearch-forward-symbol)
   ;; ("h" . isearch-forward-word)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly--tab-key-map)
 ;; This keymap I've not used. things are here experimentally.
 ;; The TAB key is not in a very good ergonomic position on average keyboards, so 【leader tab ‹somekey›】 probably should not be used much.
 ;; Currently (2018-03-13), these are commands related to completion or indent, and I basically never use any of these (except sometimes complete-symbol).
 ;; For average user, the way it is now is probably justified, because most emacs users don't use these commands.
 ;; To customize this keymap see http://ergoemacs.org/misc/xah-fly-keys_customization.html.
 '(
   ("TAB" . indent-for-tab-command)

   ("i" . complete-symbol)
   ("g" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)

   ;; temp
   ("1" . abbrev-prefix-mark)
   ("2" . edit-abbrevs)
   ("3" . expand-abbrev)
   ("4" . expand-region-abbrevs)
   ("5" . unexpand-abbrev)
   ("6" . add-global-abbrev)
   ("7" . add-mode-abbrev)
   ("8" . inverse-add-global-abbrev)
   ("9" . inverse-add-mode-abbrev)
   ("0" . expand-jump-to-next-slot)
   ("=" . expand-jump-to-previous-slot)))

;; HHH___________________________________________________________________

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-c-keymap)
 '(
   ("," . xah-open-in-external-app)
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("e" . ibuffer)
   ("f" . xah-open-recently-closed)
   ("g" . xah-open-in-terminal)
   ("h" . recentf-open-files)
   ("i" . xah-copy-file-path)
   ("l" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("o" . xah-show-in-desktop)
   ("p" . xah-open-last-closed)
   ("s" . write-file)
   ("u" . xah-open-file-at-cursor)
   ("y" . xah-list-recently-closed)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-e-keymap)
 '(
   ("RET" . insert-char)

   ("c" . xah-insert-ascii-single-quote)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace)
   ("l" . xah-insert-formfeed)
   ("n" . xah-insert-square-bracket)
   ("t" . xah-insert-paren)))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-h-keymap)
 '(
   ;; ',.
   ;; ;
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
   ("o" . describe-language-environment)
   ;; p
   ;; q
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ;; t
   ("u" . elisp-index-search)
   ("v" . apropos-value)
   ;; wxy
   ("z" . describe-coding-system)))

(xah-fly--define-keys
 ;; commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-fly-n-keymap)
 '(
   ("SPC" . whitespace-mode)
   ("," . abbrev-mode)
   ("." . toggle-frame-fullscreen)
   ;; ("'" . )
   ;; (";" . )
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("4" . global-display-line-numbers-mode)
   ;; 5
   ("6" . calendar)
   ("7" . calc)
   ;; 8
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ;; f
   ;; g
   ("h" . widen)
   ("i" . make-frame-command)
   ("j" . flyspell-buffer)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ("m" . jump-to-register)
   ("n" . narrow-to-region)
   ("o" . variable-pitch-mode)
   ("p" . read-only-mode)
   ;; q
   ;; 2021-08-10 xah-fly-keys, maybe add these
   ("r" . count-words)
   ("s" . count-matches)
   ("t" . narrow-to-defun)
   ("u" . shell)
   ("v" . visual-line-mode)
   ("w" . eww)
   ("x" . save-some-buffers)
   ("y" . toggle-truncate-lines)
   ("z" . abort-recursive-edit)))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-r-keymap)
 '(("SPC" . rectangle-mark-mode)
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("3" . number-to-register)
   ("4" . increment-register)
   ;; a b
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("e" . call-last-kbd-macro)
   ;; f
   ("g" . kill-rectangle)
   ("l" . clear-rectangle)
   ;; m
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("p" . kmacro-end-macro)
   ;; q
   ("r" . yank-rectangle)
   ;; s t u
   ;; v w
   ("y" . delete-whitespace-rectangle)
   ;; z
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
   ("f" . xah-search-current-word)
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

(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")

(defun xah-fly--update-key-map ()
  (setq xah-fly-key-map (if xah-fly-insert-state-q
                            xah-fly-insert-map
                          xah-fly-command-map)))

(defun xah-fly-keys-set-layout (Layout)
  "Set a keyboard layout.
Argument must be one of:

 dvorak
 qwerty

In elisp, those should be strings.

Version 2021-05-19 2021-09-17"
  (interactive
   (list
    (widget-prompt-value
     (get 'xah-fly-key-current-layout 'custom-type)
     "New keyboard layout: ")))
  (funcall (get 'xah-fly-key-current-layout 'custom-set)
           'xah-fly-key-current-layout
           Layout))

(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version 2020-04-28"
  (interactive)
  (setq xah-fly-insert-state-q nil)
  (xah-fly--update-key-map)
  (setq xah-fly--deactivate-command-mode-func
        (set-transient-map xah-fly-command-map (lambda () t)))
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  ;; (set-face-background 'cursor "red")
  (setq mode-line-front-space (concat (propertize "NORMAL" 'face '(:foreground "burlywood1" :background "firebrick4")) " "))
  (force-mode-line-update))

(defun xah-fly-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq xah-fly-insert-state-q t)
  (xah-fly--update-key-map)
  (funcall xah-fly--deactivate-command-mode-func)
  (unless no-indication
    (modify-all-frames-parameters '((cursor-type . bar)))
    ;; (set-face-background 'cursor "black")
    (setq mode-line-front-space (concat (propertize "INSERT" 'face '(:foreground "burlywood1" :background "royalblue4")) " ")))
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

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode.
Version 2017-07-07"
  (interactive)
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

;; HHH___________________________________________________________________

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout.

URL `http://ergoemacs.org/misc/ergoemacs_vi_mode.html'"
  :group 'xah-fly-keys
  :global t
  :lighter "XFK"
  :keymap xah-fly-insert-map
  (delete-selection-mode 1)
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
    ;; Teardown:
    (remove-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
    (remove-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
    (remove-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
    (remove-hook 'eshell-mode-hook 'xah-fly-insert-mode-activate)
    (remove-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
    (xah-fly-insert-mode-init :no-indication)
    (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))))

(provide 'xah-fly-keys)

;;; xah-fly-keys.el ends here
