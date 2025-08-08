;; Test file for date adjustment function
;; Run this with: emacs -Q -l lisp/date-utils.el -l lisp/test-date.el

(defun test-all-date-formats ()
  "Test all date formats"
  (let ((test-dates '("Friday 8/8/25" 
                      "8/8/25"
                      "Friday August 08, 2025"
                      "August 08, 2025"
                      "Friday August 8, 2025"
                      "August 8, 2025")))
    (dolist (date test-dates)
      (message "Testing: %s" date)
      (erase-buffer)
      (insert date)
      (goto-char (point-min))
      (message "Buffer content: '%s'" (buffer-string))
      (test-regex-patterns)
      (message "---"))))

(defun test-regex-patterns ()
  "Test the regex patterns used in the date function"
  (let ((case-fold-search t)
        (day-slash-date-regex
         "\\b\\([A-Z][a-z]+ [0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{2\\}\\)\\b")
        (slash-date-regex
         "\\b\\([0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{2\\}\\)\\b")
        (day-month-date-regex
         "\\b\\([A-Z][a-z]+ [A-Z][a-z]+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\)\\b")
        (month-date-regex
         "\\b\\([A-Z][a-z]+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\)\\b"))
    (beginning-of-line)
    (message "DEBUG: Testing regex patterns")
    (message "DEBUG: Line content: '%s'" (buffer-substring (line-beginning-position) (line-end-position)))
    
    ;; Test each pattern
    (cond
     ((re-search-forward day-slash-date-regex (line-end-position) t)
      (message "DEBUG: Matched day-slash format: '%s'" (match-string 1)))
     ((re-search-forward slash-date-regex (line-end-position) t)
      (message "DEBUG: Matched slash format: '%s'" (match-string 1)))
     ((re-search-forward day-month-date-regex (line-end-position) t)
      (message "DEBUG: Matched day-month format: '%s'" (match-string 1)))
     ((re-search-forward month-date-regex (line-end-position) t)
      (message "DEBUG: Matched month format: '%s'" (match-string 1)))
     (t (message "DEBUG: No pattern matched")))))

;; Test the actual date adjustment function
(defun test-date-adjustment ()
  "Test the actual date adjustment function"
  (interactive)
  (let ((test-dates '("Friday 8/8/25" 
                      "8/8/25"
                      "Friday August 08, 2025"
                      "August 08, 2025"
                      "Friday August 8, 2025"
                      "August 8, 2025")))
    (dolist (date test-dates)
      (message "Testing adjustment: %s" date)
      (erase-buffer)
      (insert date)
      (goto-char (point-min))
      (message "Before: '%s'" (buffer-string))
      ;; Simulate the adjustment by calling the function
      (adjust-date-on-line 1)
      (message "After: '%s'" (buffer-string))
      (message "---"))))

;; Create a test buffer and test all formats
(find-file "test-date-buffer")
(erase-buffer)
(message "Testing all date formats:")
(test-all-date-formats)
