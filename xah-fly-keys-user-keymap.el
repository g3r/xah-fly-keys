;; -*- coding: utf-8 -*-

;; this is your personal keys. put whatever in this file.

(when xfk-use-xah-keys-p

  (progn
    ;; this should reserved for user-defined keys
    (define-prefix-command 'xah-user-keymap)

    (define-key xah-user-keymap (kbd "SPC") 'xah-dump-keymap)
    (define-key xah-user-keymap (kbd "RET") 'xah-shell-commands)

    (define-key xah-user-keymap (kbd "-") 'xah-insert-form-feed)
    (define-key xah-user-keymap (kbd ".") 'xah-title-case-region-or-line)

    (define-key xah-user-keymap (kbd "1") 'xah-copy-to-register-1)
    (define-key xah-user-keymap (kbd "2") 'xah-paste-from-register-1)
    (define-key xah-user-keymap (kbd "3") 'xah-asciify-region)
    (define-key xah-user-keymap (kbd "4") 'xah-insert-word-3)
    (define-key xah-user-keymap (kbd "5") nil)
    (define-key xah-user-keymap (kbd "6") nil)
    (define-key xah-user-keymap (kbd "7") nil)
    (define-key xah-user-keymap (kbd "8") 'xah-find-count)
    (define-key xah-user-keymap (kbd "9") 'xah-find-replace-text-regex)
    (define-key xah-user-keymap (kbd "0") 'xah-find-text-regex)

    (define-key xah-user-keymap (kbd "a") 'xah-toggle-previous-letter-case)
    (define-key xah-user-keymap (kbd "b") nil)
    (define-key xah-user-keymap (kbd "c") 'xah-cite)
    (define-key xah-user-keymap (kbd "d") 'xah-insert-date)
    (define-key xah-user-keymap (kbd "e") 'xah-open-file-fast)
    (define-key xah-user-keymap (kbd "g") 'xah-find-text)
    (define-key xah-user-keymap (kbd "f f") 'xah-dired-scale-image)
    (define-key xah-user-keymap (kbd "f r") 'xah-dired-2png)
    (define-key xah-user-keymap (kbd "f c") 'xah-dired-2jpg)
    (define-key xah-user-keymap (kbd "f b") 'xah-dired-crop-image)
    (define-key xah-user-keymap (kbd "f d") 'xah-dired-image-autocrop)
    (define-key xah-user-keymap (kbd "f g") 'xah-dired-2drawing)
    (define-key xah-user-keymap (kbd "f e") 'xah-dired-show-metadata)
    (define-key xah-user-keymap (kbd "f u") 'xah-dired-remove-all-metadata)

    (define-key xah-user-keymap (kbd "h") nil)

    (define-key xah-user-keymap (kbd "i n") 'xah-insert-random-number)
    (define-key xah-user-keymap (kbd "i s") 'xah-insert-random-string)
    (define-key xah-user-keymap (kbd "i u") 'xah-insert-random-uuid)
    (define-key xah-user-keymap (kbd "i t") 'xah-insert-random-hex)

    (define-key xah-user-keymap (kbd "j") nil)
    (define-key xah-user-keymap (kbd "j c") 'xah-words-add-comment)
    (define-key xah-user-keymap (kbd "j d") 'xah-words-add-definition)
    (define-key xah-user-keymap (kbd "j e") 'xah-words-word-etymology-linkify)
    (define-key xah-user-keymap (kbd "j f") 'xah-words-find-word-usage)
    (define-key xah-user-keymap (kbd "j h") 'xah-words-annotate)
    (define-key xah-user-keymap (kbd "j i") 'xah-words-insert-word-entry)
    (define-key xah-user-keymap (kbd "j m") 'xah-words-move-word-to-page)
    (define-key xah-user-keymap (kbd "j n") 'xah-words-new-word-entry)
    (define-key xah-user-keymap (kbd "j q") 'xah-words-query-find-then-bold)
    (define-key xah-user-keymap (kbd "j s") 'xah-words-add-source)
    (define-key xah-user-keymap (kbd "j t") 'xah-words-chinese-linkify)
    (define-key xah-user-keymap (kbd "j u") 'xah-words-search-next-unbold)

    (define-key xah-user-keymap (kbd "k") 'xah-clean-whitespace-and-save)
    (define-key xah-user-keymap (kbd "l") nil)
    (define-key xah-user-keymap (kbd "m") 'magit-status)
    (define-key xah-user-keymap (kbd "n") 'xah-make-backup)
    (define-key xah-user-keymap (kbd "o") 'xah-open-file-from-clipboard)
    (define-key xah-user-keymap (kbd "p") 'xah-copy-file-path)
    (define-key xah-user-keymap (kbd "q") 'xah-replace-BOM-mark-etc)

    (define-key xah-user-keymap (kbd "r '") 'xah-latex-to-unicode)
    (define-key xah-user-keymap (kbd "r ,") 'xah-remove-punctuation-trailing-redundant-space )
    (define-key xah-user-keymap (kbd "r .") 'xah-convert-english-chinese-punctuation)
    (define-key xah-user-keymap (kbd "r [") 'xah-remove-square-brackets)
    (define-key xah-user-keymap (kbd "r b") 'xah-change-bracket-pairs)
    (define-key xah-user-keymap (kbd "r c") 'xah-escape-quotes)
    (define-key xah-user-keymap (kbd "r d") 'xah-fix-datetime-stamp)
    (define-key xah-user-keymap (kbd "r e") 'xah-replace-straight-quotes)
    (define-key xah-user-keymap (kbd "r g") 'xah-convert-latin-alphabet-gothic)
    (define-key xah-user-keymap (kbd "r l") 'xah-unescape-quotes)
    (define-key xah-user-keymap (kbd "r p") 'xah-convert-asian/ascii-space)
    (define-key xah-user-keymap (kbd "r p") 'xah-replace-profanity)
    (define-key xah-user-keymap (kbd "r t") 'xah-twitterfy)
    (define-key xah-user-keymap (kbd "r u") 'xah-replace-newline-whitespaces-to-space)
    (define-key xah-user-keymap (kbd "r w") 'xah-convert-fullwidth-chars)
    (define-key xah-user-keymap (kbd "r x") 'xah-remove-quotes-or-brackets)

    (define-key xah-user-keymap (kbd "s") nil)
    (define-key xah-user-keymap (kbd "t") nil)
    (define-key xah-user-keymap (kbd "u") 'xah-find-replace-text)
    (define-key xah-user-keymap (kbd "v") nil)
    (define-key xah-user-keymap (kbd "w") nil)
    (define-key xah-user-keymap (kbd "y") 'xah-open-last-closed)
    (define-key xah-user-keymap (kbd "z") nil))

  (progn
    ;; command dump. temp, rare, or whatever. put them here to have a key for now. worry later
    (define-prefix-command 'xah-dump-keymap)

    (define-key xah-dump-keymap (kbd "a") nil)
    (define-key xah-dump-keymap (kbd "b") nil)
    (define-key xah-dump-keymap (kbd "c") 'xah-css-mode)
    (define-key xah-dump-keymap (kbd "d") nil)
    (define-key xah-dump-keymap (kbd "e") 'xah-elisp-mode)
    (define-key xah-dump-keymap (kbd "f") nil)
    (define-key xah-dump-keymap (kbd "g") nil)
    (define-key xah-dump-keymap (kbd "h") 'xah-html-mode)
    (define-key xah-dump-keymap (kbd "i") nil)
    (define-key xah-dump-keymap (kbd "j") 'xah-js-mode)
    (define-key xah-dump-keymap (kbd "k") nil)
    (define-key xah-dump-keymap (kbd "l") 'xah-scan-list)

    (define-key xah-dump-keymap (kbd "l") 'xah-print-text-block)
    (define-key xah-dump-keymap (kbd "m") 'xah-find-count)
    (define-key xah-dump-keymap (kbd "n") 'xah-find-replace-text-regex)
    (define-key xah-dump-keymap (kbd "o") 'xah-find-replace-text)
    (define-key xah-dump-keymap (kbd "p") 'xah-find-text-regex)
    (define-key xah-dump-keymap (kbd "p") 'xah-find-text)
    (define-key xah-dump-keymap (kbd "q") 'xah-print-file-count)
    (define-key xah-dump-keymap (kbd "r") nil)
    (define-key xah-dump-keymap (kbd "s") nil)
    (define-key xah-dump-keymap (kbd "t") 'xah-clojure-mode)
    (define-key xah-dump-keymap (kbd "u") nil)
    (define-key xah-dump-keymap (kbd "v") nil)
    (define-key xah-dump-keymap (kbd "w") nil)
    (define-key xah-dump-keymap (kbd "x") nil)
    (define-key xah-dump-keymap (kbd "y") nil)
    (define-key xah-dump-keymap (kbd "z") nil)))