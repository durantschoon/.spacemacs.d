;; Dropbox Paper utilities for Spacemacs
;; 
;; This file contains utilities for working with Dropbox Paper content in Emacs.
;; The dp- prefix stands for "Dropbox Paper".
;;
;; Functions provided:
;; - dp-icon-alt-to-unicode: Convert Dropbox Paper emoji markdown to Unicode
;; - dp-icon-convert-buffer-to-unicode: Convert entire buffer's emoji markdown
;;
;; These functions help convert Dropbox Paper's emoji markdown format:
;; ![emoji-name](https://paper.dropboxstatic.com/static/img/ace/emoji/...)
;; into Unicode emoji characters for better display and editing.

;; Conditionally load dp-cldr-emoji-data if it exists
(let ((emoji-data-file (expand-file-name "dp-cldr-emoji-data.el" (file-name-directory load-file-name))))
  (when (file-exists-p emoji-data-file)
    (load-file emoji-data-file)))

;; Alist mapping Dropbox Paper alt-texts to their canonical CLDR names
(defvar dp-emoji-aliases
  '(
    ;; Punctuation / symbols
    ("question mark" . "red question mark")
    ("cross mark" . "cross mark button")
    ("multiplication sign" . "cross mark button")
    ("check mark" . "check mark button")
    ("exclamation mark" . "red exclamation mark")
    ;; ("down arrow" . "downwards button")
    ;; ("left arrow" . "leftwards button")
    ;; ("right arrow" . "rightwards button")
    ;; ("up arrow" . "upwards button")
    ("info" . "information")

    ;; Objects
    ("calendar" . "spiral calendar")
    ("clock" . "mantelpiece clock")
    ("eye" . "eye")
    ("magnifying glass" . "magnifying glass tilted left")
    ("magnifying glass tilted right" . "magnifying glass tilted right")
    ("bell" . "bell")
    ("warning" . "warning")

    ;; Activities / people
    ("person surfing" . "person surfing")
    ("person surfing: light skin tone" . "person surfing: light skin tone")
    ("person surfing: medium-light skin tone" . "person surfing: medium-light skin tone")
    ("person surfing: medium skin tone" . "person surfing: medium skin tone")
    ("person surfing: medium-dark skin tone" . "person surfing: medium-dark skin tone")
    ("person surfing: dark skin tone" . "person surfing: dark skin tone")
  )
  "Alist mapping Dropbox Paper alt-texts to their canonical CLDR names.")

(defun dp-icon-alt-to-unicode ()
  "Replace all Dropbox Paper emoji markdown in the current narrowed region or current line.
Returns a list: (REPLACED-COUNT UNMATCHED-LIST)."
  (interactive)
  (let ((re "\\(!\\[\\([^]]+\\)\\](https://paper\\.dropboxstatic\\.com/static/img/ace/emoji/[^)]+)\\)")
        (count 0)
        (unmatched '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re (point-max) t)
        (let* ((alt-text (downcase (string-trim (match-string 2))))
               (canonical (or (assoc-default alt-text dp-emoji-aliases) alt-text))
               (unicode (assoc-default canonical dp-cldr-emoji-alist)))
          (if (and unicode (stringp unicode) (> (length unicode) 0))
              (progn
                (replace-match (string-trim unicode) t t)
                (setq count (1+ count)))
            (push alt-text unmatched)))))
    (when (called-interactively-p 'any)
      (if (= count 0)
          (message "No recognizable Dropbox icons found in region/line.")
        (message "Replaced %d Dropbox icon(s)." count)))
    (list count (nreverse unmatched))))

(defun dp-icon-convert-buffer-to-unicode ()
  "Convert all Dropbox Paper emoji markdown in the current buffer to Unicode equivalents.
Uses `dp-icon-alt-to-unicode` for each line so alias mapping and unmatched reporting are consistent."
  (interactive)
  (unless (boundp 'dp-cldr-emoji-alist)
    (user-error "dp-cldr-emoji-alist is not loaded"))
  (let ((total-count 0)
        (unmatched '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (save-restriction
          ;; Narrow to the current line and run the line converter
          (narrow-to-region (line-beginning-position) (line-end-position))
          (cl-destructuring-bind (count misses) (dp-icon-alt-to-unicode)
            (setq total-count (+ total-count count))
            (setq unmatched (append misses unmatched))))
        (forward-line 1)))
    ;; Summary message
    (message "✅ Converted %d emoji(s)." total-count)
    ;; Show unmatched report if needed
    (when unmatched
      (with-current-buffer (get-buffer-create "*Emoji Conversion Report*")
        (erase-buffer)
        (insert (format "⚠️  %d unrecognized emoji alt-text(s):\n"
                        (length (delete-dups unmatched))))
        (dolist (name (delete-dups unmatched))
          (insert (format "- %s\n" name)))
        (display-buffer (current-buffer))))))

;; Load this file
(provide 'dropbox-paper-utils)
