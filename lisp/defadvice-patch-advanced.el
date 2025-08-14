;;; defadvice-patch-advanced.el --- Advanced defadvice patching with warning suppression -*- lexical-binding: t -*-

;; Author: Durant Schoon
;; Version: 1.0
;; Keywords: package, defadvice, warnings

;; This script automatically patches defadvice warnings in downloaded packages
;; by adding warning suppression to files that contain defadvice calls.
;; It handles conditional defadvice calls intelligently.

;; Suppress obsolete warnings globally
(put 'defadvice 'byte-obsolete-info nil)
(put 'destructuring-bind 'byte-obsolete-info nil)
(put 'callf 'byte-obsolete-info nil)

(defun defadvice-patch--find-elpa-files ()
  "Find all .el files in elpa directories."
  (let ((files '()))
    ;; Check if package-user-dir is already a develop directory
    (if (string-match "develop/?$" package-user-dir)
        ;; package-user-dir is already the develop directory, look for packages directly
        (dolist (dir (directory-files package-user-dir t "^[^.]"))
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t "\\.el$"))
              (push file files))))
      ;; Standard structure: look for elpa/*/develop/
      (dolist (dir (directory-files package-user-dir t "^[^.]"))
        (let ((develop-dir (expand-file-name "develop" dir)))
          (when (file-directory-p develop-dir)
            (dolist (file (directory-files develop-dir t "\\.el$"))
              (push file files))))))

    files))

(defun defadvice-patch--file-contains-defadvice-p (file)
  "Check if FILE contains defadvice calls."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\\bdefadvice\\b" nil t))))

(defun defadvice-patch--file-contains-obsolete-calls-p (file)
  "Check if FILE contains any obsolete function calls."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward "\\bdefadvice\\b" nil t)
          (re-search-forward "\\bdestructuring-bind\\b" nil t)
          (re-search-forward "\\bcallf\\b" nil t)))))

(defun defadvice-patch--insert-warning-suppression (file)
  "Insert warning suppression into FILE.
Looks for a good place to insert it, typically after the header comment."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      ;; Look for the end of the header comment
      (if (re-search-forward "^;;;.*\\.el.*---" nil t)
          (progn
            ;; Find the end of the header comment block
            (re-search-forward "^[^;]" nil t)
            (forward-line -1)
            (end-of-line)
            (insert "\n\n;; Suppress obsolete warnings\n(put 'defadvice 'byte-obsolete-info nil)\n(put 'destructuring-bind 'byte-obsolete-info nil)\n(put 'callf 'byte-obsolete-info nil)\n"))
        ;; If no header found, insert at the beginning
        (goto-char (point-min))
        (insert ";; Suppress obsolete warnings\n(put 'defadvice 'byte-obsolete-info nil)\n(put 'destructuring-bind 'byte-obsolete-info nil)\n(put 'callf 'byte-obsolete-info nil)\n\n")))
    (write-region (point-min) (point-max) file)))

(defun defadvice-patch--patch-file (file)
  "Patch a single file to eliminate defadvice warnings."
  (message "Patching %s..." (file-name-nondirectory file))
  (condition-case err
      (progn
        ;; Only insert warning suppression if it's not already there
        (unless (defadvice-patch--file-contains-warning-suppression-p file)
          (defadvice-patch--insert-warning-suppression file))
        (message "Successfully patched %s" (file-name-nondirectory file)))
    (error
     (message "Failed to patch %s: %s" (file-name-nondirectory file) err))))

(defun defadvice-patch--file-contains-warning-suppression-p (file)
  "Check if FILE already contains the warning suppression."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward ";; Suppress obsolete warnings" nil t))))

(defun defadvice-patch-all-packages ()
  "Patch all downloaded packages to eliminate obsolete warnings."
  (interactive)
  (let ((files (defadvice-patch--find-elpa-files))
        (count 0))
    (dolist (file files)
      (when (defadvice-patch--file-contains-obsolete-calls-p file)
        (defadvice-patch--patch-file file)
        (setq count (1+ count))))
    (message "Patched %d files to eliminate obsolete warnings" count)))

(defun defadvice-patch--after-package-install ()
  "Hook function to run after package installation."
  (run-with-timer 1 nil #'defadvice-patch-all-packages))

;; Add hook to run after package operations
(add-hook 'package-post-install-hook #'defadvice-patch--after-package-install)
(add-hook 'package-post-update-hook #'defadvice-patch--after-package-install)

(defun defadvice-patch--cleanup-old-patches (file)
  "Remove old warning suppression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      ;; Remove old defadvice-only suppression
      (while (re-search-forward ";; Suppress defadvice warnings" nil t)
        (let ((start (match-beginning 0))
              (end (save-excursion
                     (forward-line 1)
                     (point))))
          (delete-region start end)))
      ;; Remove legacy-defadvice macro definitions if any
      (while (re-search-forward "(defmacro legacy-defadvice[^)]*)" nil t)
        (let ((start (match-beginning 0))
              (end (save-excursion
                     (forward-sexp 1)
                     (point))))
          (delete-region start end)
          (delete-blank-lines)))
      ;; Replace legacy-defadvice calls back to defadvice
      (while (re-search-forward "\\blegacy-defadvice\\b" nil t)
        (replace-match "defadvice")))
    (write-region (point-min) (point-max) file)))

;; Interactive command to manually patch all packages
(defun defadvice-patch-manual ()
  "Manually patch all packages to eliminate obsolete warnings."
  (interactive)
  (defadvice-patch-all-packages))

;; Interactive command to cleanup old patches and reapply new approach
(defun defadvice-patch-cleanup-and-reapply ()
  "Clean up old patches and reapply the new warning suppression approach."
  (interactive)
  (let ((files (defadvice-patch--find-elpa-files))
        (count 0))
    (message "Cleaning up old patches...")
    (dolist (file files)
      (when (or (defadvice-patch--file-contains-defadvice-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "\\blegacy-defadvice\\b" nil t))))
        (defadvice-patch--cleanup-old-patches file)
        (setq count (1+ count))))
    (message "Cleaned up %d files" count)
    (message "Applying new warning suppression approach...")
    (defadvice-patch-all-packages)))

;; Provide the package
(provide 'defadvice-patch-advanced)
