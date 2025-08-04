(defun dp-generate-full-cldr-emoji-alist-to-file (outfile)
  "Fetch emoji-test.txt and save CLDR short name → emoji list as sequential setqs to OUTFILE."
  (interactive "FSave emoji data to file: ")
  (let ((url "https://unicode.org/Public/emoji/latest/emoji-test.txt")
        (url-request-extra-headers '(("Accept-Encoding" . "identity")))
        (alist '()))
    (message "Downloading emoji data from Unicode.org...")
    (with-current-buffer (url-retrieve-synchronously url t t)
      ;; Skip HTTP headers
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (let ((body-start (point)))
        ;; Must make buffer multibyte before narrowing
        (set-buffer-multibyte t)
        (narrow-to-region body-start (point-max))
        ;; Decode only the body as UTF-8
        (decode-coding-region (point-min) (point-max) 'utf-8)
        ;; Parse each fully-qualified emoji line
        (while (re-search-forward
                "^\\([0-9A-F ]+\\)\\s-*;\\s-*fully-qualified\\s-*#\\s-*\\(.+?\\)\\s-+E[0-9.]+\\s-+\\(.+\\)$"
                nil t)
          (let ((emoji (string-trim (match-string 2)))
                (name  (string-trim (match-string 3))))
            (push (cons (downcase name) emoji) alist)))))
    ;; Write as sequential setqs to avoid deep nesting
    (with-temp-file outfile
      (insert "(setq dp-cldr-emoji-alist nil)\n")
      (dolist (pair (reverse alist))
        (insert (format "(setq dp-cldr-emoji-alist (cons '%S dp-cldr-emoji-alist))\n" pair))))
    (message "✅ Saved %d entries to %s" (length alist) outfile)))

(defun dp-validate-cldr-emoji-alist ()
  "Sanity check on dp-cldr-emoji-alist."
  (interactive)
  (if (and (boundp 'dp-cldr-emoji-alist)
           (listp dp-cldr-emoji-alist)
           (> (length dp-cldr-emoji-alist) 1000))
      (message "✅ Emoji alist seems valid with %d entries." (length dp-cldr-emoji-alist))
    (message "❌ Emoji alist missing or incomplete.")))
