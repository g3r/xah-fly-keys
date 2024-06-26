;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; byte-compile-docstring-max-column: 200; -*-

;; Copyright © 2013-2022 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Maintainer: German Pacenza
;; Version: 16.15.20220314145449
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "28.2"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; License: GPL v3.
;; Homepage: http://xahlee.info/emacs/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. It is modal like
;; vi, but key choices are based on statistics of command call
;; frequency.

;; If you like this project, Buy Xah Emacs Tutorial http://xahlee.info/emacs/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;; Code:

(defgroup xah-fly-keys nil
  "Ergonomic modal keybinding minor mode."
  :group 'keyboard)

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defvar xah-fly-command-mode-indicator " NORMAL "
  "Character in mode line indicating command mode is active.")
(defvar xah-fly-insert-mode-indicator " INSERT "
  "Character in mode line indicating insert mode is active.")
(defvar xah-fly-emacs-mode-indicator " EMACS "
  "Character in mode line indicating emacs mode is active.")


(defvar xfk-inhibit-modes ()
  "List of modes where xah-fly-keys is disabled.")

(defface xfk-command-mode-indicator
  '((default :foreground "gray90" :weight bold)
    (((class color) (min-colors 88) (background light))
     :background "firebrick4")
    (((class color) (min-colors 88) (background dark))
     :background "firebrick4")
    (t :inherit highlight))
  "xah-fly-keys command mode indicator face"
  :group 'xah-fly-keys)


(defface xfk-insert-mode-indicator
  '((default :foreground "grey90" :weight bold)
    (((class color) (min-colors 88) (background light))
     :background "#105020")
    (((class color) (min-colors 88) (background dark))
     :background "#105020")
    (t :inherit highlight))
  "xah-fly-keys insert mode indicator face"
  :group 'xah-fly-keys)

(defface xfk-emacs-mode-indicator
  '((default :foreground "grey90" :weight bold)
    (((class color) (min-colors 88) (background light))
     :background "maroon4")
    (((class color) (min-colors 88) (background dark))
     :background "maroon4")
    (t :inherit highlight))
  "xah-fly-keys emacs mode indicator face"
  :group 'xah-fly-keys)


(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.
Version: 2021-08-12"
  (let ( xp1 xp2 (xblankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq xp1 (if (re-search-backward xblankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq xp2 (if (re-search-forward xblankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons xp1 xp2 )))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary,
else same as `xah-get-bounds-of-block'.
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

;; cursor movement

;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_previous_position.html'
Version: 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous block.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, beginning of line means visual line.
URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-30 2022-07-03 2022-07-06"
  (interactive)
  (let ((xp (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (when
            (re-search-backward "\n[\t\n ]*\n+" nil 1)
          (skip-chars-backward "\n\t ")
          (forward-char))
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode)
            (progn
              (declare-function eshell-bol "esh-mode.el" ())
              (eshell-bol))
          (back-to-indentation)
          (when (eq xp (point))
            (beginning-of-line)))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next block.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.
URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-05"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil 1)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

;; editing commands

(defun xah-copy-line-or-region ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region').

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_current_line.html'
Version: 2019-10-30"
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
Version: 2015-06-10"
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
Version: 2015-08-22"
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
Version: 2015-08-22"
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
Version: 2017-07-25 2020-09-08"
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

(defun xah-add-space-after-comma ()
  "Add a space after comma of current block or selection.
and highlight changes made.
Version 2022-01-20"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while
          (re-search-forward ",\\b" nil t)
        (replace-match ", ")
        (overlay-put
         (make-overlay
          (match-beginning 0)
          (match-end 0)) 'face 'highlight)))))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2018-04-02"
  (interactive)
  (let (xp3 xp4)
          (skip-chars-backward "\n")
          (setq xp3 (point))
          (skip-chars-forward "\n")
          (setq xp4 (point))
          (delete-region xp3 xp4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version: 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring spaces, then newlines, then spaces again,
leaving one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* ((xeol-count 0)
         (xp0 (point))
         xp1 ; whitespace begin
         xp2 ; whitespace end
         (xcharBefore (char-before))
         (xcharAfter (char-after))
         (xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9) (eq xcharAfter 32) (eq xcharAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq xp1 (point))
    (goto-char xp0)
    (skip-chars-forward " \n\t　")
    (setq xp2 (point))
    (goto-char xp1)
    (while (search-forward "\n" xp2 t)
      (setq xeol-count (1+ xeol-count)))
    (goto-char xp0)
    (cond
     ((eq xeol-count 0)
      (if (> (- xp2 xp1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq xeol-count 1)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq xeol-count 2)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char xp2)
          (search-backward "\n")
          (delete-region xp1 (point))
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
Version: 2020-11-22 2021-08-13"
  (interactive)
  ;; This command symbol has a property “'longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( (xisLongline (if (eq last-command this-command) (get this-command 'longline-p) t))
         (deactivate-mark nil)
         xp1 xp2 )
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if xisLongline
        (fill-region xp1 xp2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region xp1 xp2)))
    (put this-command 'longline-p (not xisLongline))))

(defun xah-reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2017-01-11 2022-01-08"
  (interactive "r")
  (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\n" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (search-forward "\t" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward " +" nil 1) (replace-match " "))
      (goto-char (point-max))))

(defun xah-reformat-to-multi-lines ( &optional Begin End MinLength)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2018-12-16 2021-07-06 2021-08-12"
  (interactive)
  (let ( xp1 xp2 xminlen )
    (setq xminlen (if MinLength MinLength (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column)))
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) xminlen)
            (replace-match "\n" )))))))

(defun xah-reformat-lines (&optional Width)
  "Reformat current block or selection into 1 long line or short lines.
When called for the first time, change to one long line. Second call change it to short lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length of line. By default, it is 70.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Created 2016 or before.
Version: 2021-07-05 2021-08-13 2022-03-12"
  (interactive)
  ;; This command symbol has a property 'is-long-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let (xisLong xwidth xp1 xp2)
    (setq xwidth (if Width Width (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 70)))
    (setq xisLong (if (eq last-command this-command) (get this-command 'is-long-p) nil))
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if current-prefix-arg
        (xah-reformat-to-multi-lines xp1 xp2 xwidth)
      (if xisLong
          (xah-reformat-to-multi-lines xp1 xp2 xwidth)
        (progn
          (xah-reformat-whitespaces-to-one-space xp1 xp2))))
    (put this-command 'is-long-p (not xisLong))))

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version: 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((xlbp (line-beginning-position))
          (xlep (line-end-position)))
      (if (eq xlbp xlep)
          (progn
            (comment-dwim nil))
        (if (eq (point) xlep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region xlbp xlep)
            (forward-line )))))))

;; insertion commands

(defun g3r/open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (xah-fly-insert-mode-activate))

;; text selection

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2017-11-01 2021-03-19"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
                (end-of-visual-line 1)
                (when (eq xp1 (point))
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

;; misc

(defun xah-user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user buffer.
This function is used by buffer switching command and close buffer
command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version: 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-equal major-mode "help-mode") nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
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
Version: 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;; key maps for conversion

(defmacro xah-fly--define-keys (KeymapName KeyCmdAlist)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
Version: 2022-08-10"
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName , KeymapName))
       ,@(mapcar
          (lambda (xpair)
            `(define-key
               ,xkeymapName
               ,(car xpair)
               ,(list 'quote (cdr xpair))))
          (cadr KeyCmdAlist)))))

;; keymaps

(defvar-keymap xah-fly-key-map
  :doc "Backward-compatibility map for `xah-fly-keys' minor mode.")

(defvar-keymap xah-fly-command-map
  :doc "Keymap that takes precedence over all other keymaps in command mode.")

(defvar-keymap xah-fly-insert-map
  :doc "Keymap for bindings that will be checked in insert mode.")

(defvar xah-fly--deactivate-command-mode-func nil)

;; setting keys

(xah-fly--define-keys
 xah-fly-command-map
 '(
   ("~" . nil)
   (":" . nil)

   (" " . xah-fly-leader-key-map) ;; SPC
   ("" . xah-fly-leader-key-map) ;; DEL
   ("'" . xah-reformat-lines)
   ("," . xah-shrink-whitespaces)
   ("." . backward-kill-word)
   (";" . comment-line)
   ("/" . hippie-expand)
   ("\\" . nil)
   ("=" . nil)

   ("2" . xah-select-line)
   ("3" . delete-other-windows)
   ("4" . split-window-below)

   ("a" . execute-extended-command)
   ("b" . isearch-forward)
   ("B" . query-replace)
   ("c" . previous-line)
   ("d" . xah-beginning-of-line-or-block)
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
   ("o" . g3r/open-line-below)
   ("p" . kill-word)
   ("q" . xah-cut-line-or-region)
   ("r" . forward-word)
   ("s" . xah-end-of-line-or-block)
   ("t" . next-line)
   ("u" . xah-fly-insert-mode-activate)
   ("v" . forward-list)
   ("w" . other-window)
   ("y" . set-mark-command)
   ("z" . universal-argument)))

;; set control meta, etc keys

(xah-fly--define-keys
 xah-fly-insert-map
 '(([home] . xah-fly-command-mode-activate)))

;; commands related to highlight
;; (xah-fly--define-keys
 ;; (define-prefix-command 'xah-fly-dot-keymap)
 ;; '(
   ;; ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-c-keymap)
 '(
   ("." . find-file)
   ("," . find-file-other-window)
   ("c" . bookmark-bmenu-list)
   ("l" . bookmark-set)
   ("r" . bookmark-jump)
   ))

;; (xah-fly--define-keys
 ;; (define-prefix-command 'xah-fly-e-keymap)
 ;; '(
   ;; ))

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
   ("o" . describe-symbol)
   ("r" . apropos-variable)
   ("u" . elisp-index-search)
   ))

(xah-fly--define-keys
 ;; commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-fly-n-keymap)
 '(
   (" " . whitespace-mode)
   (","   . abbrev-mode)
   ("."   . toggle-frame-fullscreen)
   ("2"   . global-hl-line-mode)
   ("3"   . visual-line-mode)
   ("4"   . global-display-line-numbers-mode)
   ("e"   . eshell)
   ("o"   . variable-pitch-mode)
   ("u"   . shell)
   ("v"   . visual-line-mode)
   ("y"   . toggle-truncate-lines)
   ))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-r-keymap)
 '((" " . rectangle-mark-mode) ;; SPC
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
   ("." . sort-lines)
   ("d" . mark-defun)
   ("e" . list-matching-lines)
   ("h" . xah-pop-local-mark-ring)
   ("u" . ibuffer)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-w-keymap)
 '(
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("o" . window-swap-states)
   ("p" . eval-expression)
   ("q" . save-buffers-kill-terminal)
   ("u" . eval-region)
))

;; (xah-fly--define-keys
;;  ;; kinda replacement related
;;  (define-prefix-command 'xah-fly-comma-keymap)
;;  '(
;;    ("t" . xref-find-definitions)
;;    ("n" . xref-pop-marker-stack)))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-leader-key-map)
 '(
   (" " . project-find-file) ;; SPC
   ([home] . keyboard-escape-quit)
   ;; ("." . xah-fly-dot-keymap)
   ("'" . xah-fill-or-unfill)
   ("," . xref-go-back)
   ("." . xref-find-definitions)
   
   ("3" . delete-window)
   ("4" . split-window-right)
   ("5" . balance-windows)

   ("a" . mark-whole-buffer)
   ("b" . end-of-buffer)
   ("c" . xah-fly-c-keymap)
   ("d" . beginning-of-buffer)
   ;; ("e" . xah-fly-e-keymap)
   ("h" . xah-fly-h-keymap)
   ("i" . kill-line)
   ("l" . recenter-top-bottom)
   ("m" . dired-jump)
   ("n" . xah-fly-n-keymap)
   ("o" . exchange-point-and-mark)
   ("p" . project-prefix-map)
   ("r" . xah-fly-r-keymap)
   ("s" . save-buffer)
   ("t" . xah-fly-t-keymap)
   ("u" . switch-to-buffer)
   ("v" . vc-prefix-map)
   ("w" . xah-fly-w-keymap)
   ))

;; Movement key integrations with built-in Emacs packages

(xah-fly--define-keys
 indent-rigidly-map
 '(("h" . indent-rigidly-left)
   ("n" . indent-rigidly-right)))

(defvar xah-fly-insert-state-p t "non-nil means insertion mode is on.")

(defun xah-fly--update-key-map ()
  (setq xah-fly-key-map (if xah-fly-insert-state-p
                            xah-fly-insert-map
                          xah-fly-command-map)))

(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version: 2022-07-06"
  (interactive)
  (setq xah-fly-insert-state-p nil)
  (xah-fly--update-key-map)
  (when xah-fly--deactivate-command-mode-func
    (funcall xah-fly--deactivate-command-mode-func))
  (setq xah-fly--deactivate-command-mode-func
        (set-transient-map xah-fly-command-map (lambda () t)))
  ;; (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (set-face-background 'cursor "firebrick1")
  (setq mode-line-front-space (propertize xah-fly-command-mode-indicator 'face 'xfk-command-mode-indicator))
  (force-mode-line-update))

(defun xah-fly-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq xah-fly-insert-state-p t)
  (xah-fly--update-key-map)
  (funcall xah-fly--deactivate-command-mode-func)
  (unless no-indication
    ;; (modify-all-frames-parameters '((cursor-type . bar)))
    (set-face-attribute 'cursor nil :background "darkgreen")
    (setq mode-line-front-space (propertize xah-fly-insert-mode-indicator 'face 'xfk-insert-mode-indicator)))
  (force-mode-line-update))

(defun xah-fly-command-mode-activate ()
  "Activate command mode and run `xah-fly-command-mode-activate-hook'
Version: 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode.
Version: 2017-07-07"
  (interactive)
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))


;;  g3r: change cursor in terminal mode

(unless (display-graphic-p)
  (add-hook 'xah-fly-insert-mode-activate-hook
            (lambda () (send-string-to-terminal "\033[6 q")))
  (add-hook 'xah-fly-command-mode-activate-hook
            (lambda () (send-string-to-terminal "\033[0 q")))
  (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\033[0 q"))))

(defun disable-xfk (&optional buf)
  (ignore buf)
  (if (or (default-value buffer-read-only) (member major-mode xfk-inhibit-modes))
      (progn
	(xah-fly-keys -1)
	(set-face-background 'cursor "dark blue"))
    (progn
      (set-face-background 'cursor "firebrick1")
      (xah-fly-keys t))))

;;;###autoload
(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles,
 like Dvorak layout.

URL `http://xahlee.info/emacs/misc/ergoemacs_vi_mode.html'"
  :group 'xah-fly-keys
  :global t
  :lighter nil
  :keymap xah-fly-insert-map
  (delete-selection-mode 1)
  (setq shift-select-mode nil)

  (if xah-fly-keys
      ;; Construction:
      (progn
        (add-hook 'isearch-mode-end-hook             #'xah-fly-command-mode-activate)
	(add-hook 'window-buffer-change-functions    #'disable-xfk)
	(add-hook 'window-selection-change-functions #'disable-xfk)
        (xah-fly-command-mode-activate))
    (progn
      ;; Teardown:
      (remove-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
      (xah-fly-insert-mode-init :no-indication)
      (setq mode-line-front-space (propertize xah-fly-emacs-mode-indicator 'face 'xfk-emacs-mode-indicator))
      ;;
      )))

(provide 'xah-fly-keys)

;;; xah-fly-keys.el ends here
