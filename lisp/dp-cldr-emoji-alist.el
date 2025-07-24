s(defun dp-generate-full-cldr-emoji-alist ()
   "Fetch emoji-test.txt and generate dp-cldr-emoji-alist."
   (interactive)
   (let ((url "https://unicode.org/Public/emoji/latest/emoji-test.txt")
         (bufname "*dp-emoji-alist*")
         (alist '()))
     (message "Downloading emoji data from Unicode.org...")
     (with-current-buffer (url-retrieve-synchronously url t)
       ;; Force UTF-8 decoding
       (set-buffer-file-coding-system 'utf-8)
       (set-buffer-multibyte t)
       (goto-char (point-min))
       ;; Skip HTTP headers
       (re-search-forward "^$" nil 'move)
       (forward-line)
       (while (not (eobp))
         (let ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
           (when (string-match
                  "^\\([0-9A-F ]+\\)\\s-*;\\s-*fully-qualified\\s-*#\\s-*\\([[:graph:] ]+\\)\\s-*E[0-9.]+\\s-*\\(.*\\)$"
                  line)
             (let ((emoji (match-string 2 line))
                   (name (match-string 3 line)))
               (push (cons name emoji) alist))))
         (forward-line)))
     (message "Download and parse complete. Writing output buffer...")
     ;; Output result buffer
     (with-current-buffer (get-buffer-create bufname)
       (erase-buffer)
       (insert "(defvar dp-cldr-emoji-alist\n  '(\n")
       (dolist (pair (reverse alist))
         (insert (format "    (\"%s\" . \"%s\")\n" (car pair) (cdr pair))))
       (insert "  )\n  \"Alist mapping CLDR short names to emojis.\")\n")
       (emacs-lisp-mode)
       (goto-char (point-min))
       (message "Done. Buffer '%s' contains %d entries." bufname (length alist))
       (display-buffer (current-buffer)))))

(defun dp-validate-cldr-emoji-alist ()
  "Sanity check on dp-cldr-emoji-alist."
  (interactive)
  (if (and (boundp 'dp-cldr-emoji-alist)
           (listp dp-cldr-emoji-alist)
           (> (length dp-cldr-emoji-alist) 1000))
      (message "✅ Emoji alist seems valid with %d entries." (length dp-cldr-emoji-alist))
    (message "❌ Emoji alist missing or incomplete.")))
