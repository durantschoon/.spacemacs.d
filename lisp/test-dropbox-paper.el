;; Test file for Dropbox Paper emoji conversion functions
;; Run this with: emacs -Q -l lisp/dropbox-paper-utils.el -l lisp/test-dropbox-paper.el

(defun test-all-dropbox-formats ()
  "Test all Dropbox Paper emoji markdown formats"
  (let ((test-cases '("![question mark](https://paper.dropboxstatic.com/static/img/ace/emoji/1f914.png)"
                      "![calendar](https://paper.dropboxstatic.com/static/img/ace/emoji/1f4c5.png)"
                      "![clock](https://paper.dropboxstatic.com/static/img/ace/emoji/1f550.png)"
                      "![eye](https://paper.dropboxstatic.com/static/img/ace/emoji/1f441.png)"
                      "![magnifying glass](https://paper.dropboxstatic.com/static/img/ace/emoji/1f50d.png)"
                      "![bell](https://paper.dropboxstatic.com/static/img/ace/emoji/1f514.png)"
                      "![warning](https://paper.dropboxstatic.com/static/img/ace/emoji/26a0.png)"
                      "![person surfing](https://paper.dropboxstatic.com/static/img/ace/emoji/1f3c4.png)"
                      "![unknown emoji](https://paper.dropboxstatic.com/static/img/ace/emoji/unknown.png)")))
    (dolist (emoji test-cases)
      (message "Testing: %s" emoji)
      (erase-buffer)
      (insert emoji)
      (goto-char (point-min))
      (message "Buffer content: '%s'" (buffer-string))
      (test-emoji-regex-patterns)
      (message "---"))))

(defun test-emoji-regex-patterns ()
  "Test the regex patterns used in the Dropbox Paper emoji function"
  (let ((re "\\(!\\[\\([^]]+\\)\\](https://paper\\.dropboxstatic\\.com/static/img/ace/emoji/[^)]+)\\)"))
    (beginning-of-line)
    (message "DEBUG: Testing emoji regex patterns")
    (message "DEBUG: Line content: '%s'" (buffer-substring (line-beginning-position) (line-end-position)))
    
    ;; Test the pattern
    (if (re-search-forward re (line-end-position) t)
        (let* ((full-match (match-string 1))
               (alt-text (match-string 2)))
          (message "DEBUG: Matched emoji format: '%s'" full-match)
          (message "DEBUG: Alt text: '%s'" alt-text)
          (message "DEBUG: Canonical name: '%s'" (or (assoc-default (downcase alt-text) dp-emoji-aliases) alt-text)))
      (message "DEBUG: No emoji pattern matched"))))

(defun test-emoji-conversion ()
  "Test the actual emoji conversion function"
  (interactive)
  (let ((test-cases '("![question mark](https://paper.dropboxstatic.com/static/img/ace/emoji/1f914.png)"
                      "![calendar](https://paper.dropboxstatic.com/static/img/ace/emoji/1f4c5.png)"
                      "![clock](https://paper.dropboxstatic.com/static/img/ace/emoji/1f550.png)"
                      "![eye](https://paper.dropboxstatic.com/static/img/ace/emoji/1f441.png)"
                      "![magnifying glass](https://paper.dropboxstatic.com/static/img/ace/emoji/1f50d.png)"
                      "![bell](https://paper.dropboxstatic.com/static/img/ace/emoji/1f514.png)"
                      "![warning](https://paper.dropboxstatic.com/static/img/ace/emoji/26a0.png)"
                      "![person surfing](https://paper.dropboxstatic.com/static/img/ace/emoji/1f3c4.png)"
                      "![unknown emoji](https://paper.dropboxstatic.com/static/img/ace/emoji/unknown.png)")))
    (dolist (emoji test-cases)
      (message "Testing conversion: %s" emoji)
      (erase-buffer)
      (insert emoji)
      (goto-char (point-min))
      (message "Before: '%s'" (buffer-string))
      ;; Simulate the conversion by calling the function
      (dp-icon-alt-to-unicode)
      (message "After: '%s'" (buffer-string))
      (message "---"))))

(defun test-buffer-conversion ()
  "Test the buffer-wide conversion function"
  (interactive)
  (let ((test-content "Line 1: ![question mark](https://paper.dropboxstatic.com/static/img/ace/emoji/1f914.png)
Line 2: ![calendar](https://paper.dropboxstatic.com/static/img/ace/emoji/1f4c5.png)
Line 3: ![clock](https://paper.dropboxstatic.com/static/img/ace/emoji/1f550.png)
Line 4: ![unknown emoji](https://paper.dropboxstatic.com/static/img/ace/emoji/unknown.png)"))
    (message "Testing buffer conversion:")
    (erase-buffer)
    (insert test-content)
    (message "Before conversion:")
    (message "%s" (buffer-string))
    (message "---")
    ;; Call the buffer conversion function
    (dp-icon-convert-buffer-to-unicode)
    (message "After conversion:")
    (message "%s" (buffer-string))))

(defun test-emoji-aliases ()
  "Test the emoji alias mapping"
  (message "Testing emoji aliases:")
  (dolist (alias dp-emoji-aliases)
    (message "  %s -> %s" (car alias) (cdr alias))))

;; Create a test buffer and run all tests
(find-file "test-dropbox-paper-buffer")
(erase-buffer)
(message "Testing all Dropbox Paper emoji formats:")
(test-all-dropbox-formats)
(message "")
(message "Testing emoji aliases:")
(test-emoji-aliases)
(message "")
(message "Testing individual conversions:")
(test-emoji-conversion)
(message "")
(message "Testing buffer conversion:")
(test-buffer-conversion)
