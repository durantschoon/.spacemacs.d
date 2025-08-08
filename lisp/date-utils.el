;; Date adjustment utilities for Spacemacs
;; Provides functions to adjust dates on the current line while preserving format

(defun find-date-on-line ()
  "Find the first recognizable date on the current line.
Returns (START END DATE-OBJECT HAS-DAY-OF-WEEK) or nil if no date found.
Supports formats: 'Day Month D, YYYY' and 'Day M/D/YY'."
  (let ((case-fold-search t)
        (day-month-date-regex
         "\\b\\([A-Z][a-z]+ [A-Z][a-z]+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\)\\b")
        (day-slash-date-regex
         "\\b\\([A-Z][a-z]+ [0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{2\\}\\)\\b")
        (month-date-regex
         "\\b\\([A-Z][a-z]+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\)\\b")
        (slash-date-regex
         "\\b\\([0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{2\\}\\)\\b"))
    (beginning-of-line)
    (cond
     ;; Day Month D, YYYY (e.g., "Friday August 08, 2025")
     ((re-search-forward day-month-date-regex (line-end-position) t)
      (let* ((date-str (match-string 1))
             (start-pos (match-beginning 1))
             (end-pos (match-end 1))
             (parts (split-string date-str " "))
             (day-name (car parts))
             (date-without-day (string-join (cdr parts) " "))
             (parsed (parse-time-string date-without-day)))
        (message "DEBUG: Found day-month date: '%s' -> '%s' -> %s" date-str date-without-day parsed)
        (if parsed
            (list start-pos end-pos parsed t)
          nil)))
     ;; Day M/D/YY (e.g., "Friday 8/8/25")
     ((re-search-forward day-slash-date-regex (line-end-position) t)
      (let* ((date-str (match-string 1))
             (start-pos (match-beginning 1))
             (end-pos (match-end 1)))
        (message "DEBUG: Match found - start=%s, end=%s, date-str='%s'" start-pos end-pos date-str)
        (let* ((parts (split-string date-str " "))
               (day-name (car parts))
               (date-part (cadr parts))
               (date-parts (split-string date-part "/"))
               (month (string-to-number (car date-parts)))
               (day (string-to-number (cadr date-parts)))
               (year (string-to-number (caddr date-parts)))
               (century (if (< year 100) (+ 2000 year) year))
               (parsed (encode-time 0 0 0 day month century)))
          (message "DEBUG: Found day-slash date: '%s' -> parsed as %s" date-str parsed)
          (list start-pos end-pos parsed t))))
     ;; Month D, YYYY (without day of week)
     ((re-search-forward month-date-regex (line-end-position) t)
      (let* ((date-str (match-string 1))
             (start-pos (match-beginning 1))
             (end-pos (match-end 1))
             (parsed (parse-time-string date-str)))
        (message "DEBUG: Found month date: '%s' -> %s" date-str parsed)
        (if parsed
            (list start-pos end-pos parsed nil)
          nil)))
     ;; M/D/YY (without day of week)
     ((re-search-forward slash-date-regex (line-end-position) t)
      (let* ((date-str (match-string 1))
             (start-pos (match-beginning 1))
             (end-pos (match-end 1))
             (date-parts (split-string date-str "/"))
             (month (string-to-number (car date-parts)))
             (day (string-to-number (cadr date-parts)))
             (year (string-to-number (caddr date-parts)))
             (century (if (< year 100) (+ 2000 year) year))
             (parsed (encode-time 0 0 0 day month century)))
        (message "DEBUG: Found slash date: '%s' -> parsed as %s" date-str parsed)
        (list start-pos end-pos parsed nil))
     (t (message "DEBUG: No date found on line") nil)))))

(defun adjust-date-on-line (days)
  "Adjust the first recognizable date on the current line by DAYS.
Supports formats: 'Day Month D, YYYY' and 'Day M/D/YY'."
  (interactive "nDays to adjust by: ")
  (let ((date-info (find-date-on-line)))
    (message "DEBUG: date-info = %s" date-info)
    (if date-info
        (let* ((start (nth 0 date-info))
               (end (nth 1 date-info))
               (date-obj (nth 2 date-info))
               (has-day-of-week (nth 3 date-info))
               (original-date (buffer-substring start end)))
          (message "DEBUG: start=%s, end=%s, date-obj=%s, has-day=%s" start end date-obj has-day-of-week)
          (message "DEBUG: original-date='%s'" original-date)
          (if (and date-obj start end)
              (let* ((adjusted-time (if (and (listp date-obj) (>= (length date-obj) 6))
                                        ;; parse-time-string format: (SEC MIN HOUR DAY MONTH YEAR DOW DST TZ)
                                        (encode-time 0 0 0
                                                     (+ (nth 3 date-obj) days)
                                                     (nth 4 date-obj)
                                                     (nth 5 date-obj))
                                      ;; encode-time format: need to decode and re-encode
                                      (let ((decoded (decode-time date-obj)))
                                        (encode-time 0 0 0
                                                     (+ (nth 3 decoded) days)
                                                     (nth 4 decoded)
                                                     (nth 5 decoded)))))
                     (new-date (adjust-date-format original-date adjusted-time has-day-of-week)))
                (message "DEBUG: adjusted-time=%s, new-date=%s" adjusted-time new-date)
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (insert new-date))
                (message "Adjusted date: %s" new-date))
            (message "Invalid date object or position: date-obj=%s, start=%s, end=%s" date-obj start end)))
      (message "No date found on current line"))))

(defun adjust-date-format (original-date adjusted-time has-day-of-week)
  "Format the adjusted time to match the original date format.
ORIGINAL-DATE is the original date string, ADJUSTED-TIME is the adjusted time object,
HAS-DAY-OF-WEEK indicates if the original had a day of week."
  (let* ((decoded (decode-time adjusted-time))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (weekday (nth 6 decoded)))
    (cond
     ;; Check for slash format (8/8/25 or Friday 8/8/25)
     ((string-match "\\b\\([A-Z][a-z]+ \\)?\\([0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{2\\}\\)\\b" original-date)
      (let ((day-name (if has-day-of-week
                          (format-time-string "%A " adjusted-time)
                        "")))
        (concat day-name (format-time-string "%-m/%-d/%y" adjusted-time))))
     ;; Check for month format with leading zero (August 08, 2025 or Friday August 08, 2025)
     ((string-match "\\b\\([A-Z][a-z]+ \\)?\\([A-Z][a-z]+ [0-9]\\{2\\}, [0-9]\\{4\\}\\)\\b" original-date)
      (let ((day-name (if has-day-of-week
                          (format-time-string "%A " adjusted-time)
                        "")))
        (concat day-name (format-time-string "%B %02d, %Y" adjusted-time))))
     ;; Check for month format without leading zero (August 8, 2025 or Friday August 8, 2025)
     ((string-match "\\b\\([A-Z][a-z]+ \\)?\\([A-Z][a-z]+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\)\\b" original-date)
      (let ((day-name (if has-day-of-week
                          (format-time-string "%A " adjusted-time)
                        "")))
        (concat day-name (format-time-string "%B %-d, %Y" adjusted-time))))
     ;; Default fallback
     (t (if has-day-of-week
             (format-time-string "%A %B %-d, %Y" adjusted-time)
           (format-time-string "%B %-d, %Y" adjusted-time))))))

;; Load this file
(provide 'date-utils)
